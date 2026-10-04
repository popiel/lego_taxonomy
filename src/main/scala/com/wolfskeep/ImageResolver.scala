package com.wolfskeep

import akka.actor.typed.{ActorRef, Behavior, DispatcherSelector}
import akka.actor.typed.scaladsl.Behaviors
import akka.util.Timeout

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._
import com.wolfskeep.rebrickable.LDrawImageFetcherTrait

object ImageResolver {
  sealed trait Command
  final case class GetLdrawImage(colorId: Int, partNumber: String, replyTo: ActorRef[LdrawImageResponse]) extends Command
  final case class GetBricksetImageUrl(partNumber: String, elementId: Option[String], replyTo: ActorRef[BricksetImageResponse]) extends Command

  sealed trait LdrawImageResponse
  final case class LdrawImageReady(bytes: Array[Byte]) extends LdrawImageResponse
  case object LdrawImagePending extends LdrawImageResponse
  case object LdrawImageUnavailable extends LdrawImageResponse

  sealed trait BricksetImageResponse
  final case class BricksetImageResolved(url: String) extends BricksetImageResponse
  case object BricksetImagePending extends BricksetImageResponse
  case object BricksetImageUnavailable extends BricksetImageResponse

  private final case class LdrawDownloadDone(colorId: Int) extends Command
  private final case class BricksetCacheChecked(
    partNumber: String,
    elementId: Option[String],
    replyTo: ActorRef[BricksetImageResponse],
    negativeChecked: Boolean,
    response: DiskCache.Response
  ) extends Command
  private case class BricksetResolveDone(partNumber: String) extends Command

  private def positiveKey(partNumber: String): String = s"brickset-image/$partNumber"
  private def negativeKey(partNumber: String): String = s"brickset-image-missing/$partNumber"

  private case class State(
    ldrawInFlight: Set[Int] = Set.empty,
    bricksetChecking: Set[String] = Set.empty,
    bricksetResolving: Set[String] = Set.empty
  )

  def apply(
    ldrawFetcher: LDrawImageFetcherTrait,
    downloader: ActorRef[CachedDownloader.Command],
    cache: ActorRef[DiskCache.Command],
    negativeTtl: FiniteDuration = Timeouts.web.imageNegativeTtl
  ): Behavior[Command] = Behaviors.setup { context =>
    implicit val ec: ExecutionContext = context.executionContext
    implicit val scheduler: akka.actor.typed.Scheduler = context.system.scheduler
    implicit val askTimeout: Timeout = Timeout(90.seconds)
    val blockingEc: ExecutionContext =
      context.system.dispatchers.lookup(DispatcherSelector.fromConfig("akka.actor.default-blocking-io-dispatcher"))

    def negativeMarkerExpired(insertedAt: Long): Boolean =
      System.currentTimeMillis() - insertedAt > negativeTtl.toMillis

    def startLdrawDownload(state: State, colorId: Int): Behavior[Command] = {
      Future(ldrawFetcher.ensureDownloaded(colorId)(blockingEc))(blockingEc)
        .onComplete(_ => context.self ! LdrawDownloadDone(colorId))
      running(state.copy(ldrawInFlight = state.ldrawInFlight + colorId))
    }

    def checkCache(state: State, partNumber: String, elementId: Option[String], replyTo: ActorRef[BricksetImageResponse]): Behavior[Command] = {
      context.ask(cache, (reply: ActorRef[DiskCache.Response]) => DiskCache.Fetch(positiveKey(partNumber), reply)) {
        case scala.util.Success(response) =>
          BricksetCacheChecked(partNumber, elementId, replyTo, negativeChecked = false, response)
        case scala.util.Failure(ex) =>
          context.log.warn(s"ImageResolver: cache lookup failed for part $partNumber: ${ex.getMessage}")
          BricksetCacheChecked(partNumber, elementId, replyTo, negativeChecked = false, DiskCache.NotFound(positiveKey(partNumber)))
      }
      running(state.copy(bricksetChecking = state.bricksetChecking + partNumber))
    }

    def checkNegativeCache(state: State, partNumber: String, elementId: Option[String], replyTo: ActorRef[BricksetImageResponse]): Behavior[Command] = {
      context.ask(cache, (reply: ActorRef[DiskCache.Response]) => DiskCache.Fetch(negativeKey(partNumber), reply)) {
        case scala.util.Success(response) =>
          BricksetCacheChecked(partNumber, elementId, replyTo, negativeChecked = true, response)
        case scala.util.Failure(ex) =>
          context.log.warn(s"ImageResolver: negative-cache lookup failed for part $partNumber: ${ex.getMessage}")
          BricksetCacheChecked(partNumber, elementId, replyTo, negativeChecked = true, DiskCache.NotFound(negativeKey(partNumber)))
      }
      Behaviors.same
    }

    def startBricksetResolve(state: State, partNumber: String, elementId: Option[String]): Behavior[Command] = {
      BricksetPartFetcher.fetchPartDetails(downloader, partNumber, elementId)
        .onComplete { result =>
          val imageUrl = result.toOption.flatten.flatMap(_.imageUrl)
          val (key, value) = imageUrl match {
            case Some(url) => (positiveKey(partNumber), url)
            case None      => (negativeKey(partNumber), "1")
          }
          cache ! DiskCache.Insert(key, value)
          context.self ! BricksetResolveDone(partNumber)
        }
      running(state.copy(
        bricksetChecking = state.bricksetChecking - partNumber,
        bricksetResolving = state.bricksetResolving + partNumber
      ))
    }

    def running(state: State): Behavior[Command] = Behaviors.receiveMessage {
      case GetLdrawImage(colorId, partNumber, replyTo) =>
        if (state.ldrawInFlight.contains(colorId)) {
          replyTo ! LdrawImagePending
          Behaviors.same
        } else if (ldrawFetcher.isZipAvailable(colorId)) {
          ldrawFetcher.getImageFromZip(colorId, partNumber) match {
            case Some(bytes) => replyTo ! LdrawImageReady(bytes)
            case None        => replyTo ! LdrawImageUnavailable
          }
          Behaviors.same
        } else if (!ldrawFetcher.canRetryDownload(colorId)) {
          replyTo ! LdrawImageUnavailable
          Behaviors.same
        } else {
          replyTo ! LdrawImagePending
          startLdrawDownload(state, colorId)
        }

      case GetBricksetImageUrl(partNumber, elementId, replyTo) =>
        if (state.bricksetResolving.contains(partNumber) || state.bricksetChecking.contains(partNumber)) {
          replyTo ! BricksetImagePending
          Behaviors.same
        } else {
          checkCache(state, partNumber, elementId, replyTo)
        }

      case LdrawDownloadDone(colorId) =>
        running(state.copy(ldrawInFlight = state.ldrawInFlight - colorId))

      case BricksetCacheChecked(partNumber, elementId, replyTo, negativeChecked, response) =>
        if (!negativeChecked) {
          response match {
            case DiskCache.FetchResult(_, url, _) =>
              replyTo ! BricksetImageResolved(url)
              running(state.copy(bricksetChecking = state.bricksetChecking - partNumber))
            case DiskCache.NotFound(_) =>
              checkNegativeCache(state, partNumber, elementId, replyTo)
          }
        } else {
          response match {
            case DiskCache.FetchResult(_, _, insertedAt) if negativeMarkerExpired(insertedAt) =>
              replyTo ! BricksetImagePending
              startBricksetResolve(state, partNumber, elementId)
            case DiskCache.FetchResult(_, _, _) =>
              replyTo ! BricksetImageUnavailable
              running(state.copy(bricksetChecking = state.bricksetChecking - partNumber))
            case DiskCache.NotFound(_) =>
              replyTo ! BricksetImagePending
              startBricksetResolve(state, partNumber, elementId)
          }
        }

      case BricksetResolveDone(partNumber) =>
        running(state.copy(bricksetResolving = state.bricksetResolving - partNumber))
    }

    running(State())
  }
}
