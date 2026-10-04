package com.wolfskeep

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.AskPattern._
import akka.util.Timeout
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Try
import com.wolfskeep.rebrickable.RebrickableHolder
import com.wolfskeep.rebrickable.LDrawImageFetcherTrait

object PartsProcessor {
  sealed trait Command
  case class ProcessParts(coloredParts: List[ColoredPart], replyTo: ActorRef[Response]) extends Command

  sealed trait Response
  case class ProcessedParts(parts: List[MatchedPart]) extends Response

  def apply(
    taxonomyDataHolder: ActorRef[TaxonomyHolder.Command],
    downloader: ActorRef[CachedDownloader.Command],
    rebrickableDataActor: ActorRef[RebrickableHolder.Command],
    ldrawImageFetcher: LDrawImageFetcherTrait
  ): Behavior[Command] = {
    Behaviors.setup { context =>
      implicit val ec: ExecutionContext = context.executionContext
      implicit val scheduler: akka.actor.typed.Scheduler = context.system.scheduler
      implicit val classicScheduler: akka.actor.Scheduler = context.system.classicSystem.scheduler
      val logger = context.log

      Behaviors.receiveMessage {
        case ProcessParts(coloredParts, replyTo) =>
          logger.info(s"ProcessParts with ${coloredParts.size} input parts")
          implicit val lookupTimeout: Timeout = Timeout(5.seconds)

          val requests = coloredParts.map(cp => TaxonomyHolder.LookupPartRequest(cp.partNumber, cp.elementId, cp.name))
          val lookupFuture = taxonomyDataHolder.ask(ref => TaxonomyHolder.LookupParts(requests, ref))

          lookupFuture.onComplete {
            case scala.util.Success(results) =>
              val paired = coloredParts.zip(results)
              val (exact, nonExact) = paired.partition { case (_, result) =>
                result.via == TaxonomyHolder.Exact || result.via == TaxonomyHolder.AltNumber
              }

              if (nonExact.isEmpty) {
                logger.info(s"ProcessParts replying with ${exact.size} output parts")
                replyTo ! ProcessedParts(exact.map { case (cp, result) => buildMatchedPart(cp, result) }.sorted)
              } else {
                processNonExactParts(exact, nonExact, rebrickableDataActor, downloader, ldrawImageFetcher, replyTo)(ec, scheduler, classicScheduler, logger)
              }

            case scala.util.Failure(ex) =>
              logger.error(s"Failed to get taxonomy data: ${ex.getMessage}")
              replyTo ! ProcessedParts(Nil)
          }

          Behaviors.same
      }
    }
  }

  private def buildMatchedPart(coloredPart: ColoredPart, result: TaxonomyHolder.LookupResult): MatchedPart =
    MatchedPart(coloredPart, result.legoPart, result.categoriesGuessed)

  private def processNonExactParts(
    exact: List[(ColoredPart, TaxonomyHolder.LookupResult)],
    nonExact: List[(ColoredPart, TaxonomyHolder.LookupResult)],
    rebrickableDataActor: ActorRef[RebrickableHolder.Command],
    downloader: ActorRef[CachedDownloader.Command],
    ldrawImageFetcher: LDrawImageFetcherTrait,
    replyTo: ActorRef[ProcessedParts]
  )(implicit ec: ExecutionContext, scheduler: akka.actor.typed.Scheduler, classicScheduler: akka.actor.Scheduler, logger: org.slf4j.Logger): Unit = {
    nonExact.foreach {
      case (cp, result) if result.via == TaxonomyHolder.Miss =>
        logger.warn(s"Failed to match part number ${cp.partNumber}, element ${cp.elementId.getOrElse("")}: ${cp.name}")
      case _ => ()
    }

    def fallback: List[MatchedPart] =
      exact.map { case (cp, result) => buildMatchedPart(cp, result) } ++
        nonExact.map { case (cp, result) => buildMatchedPart(cp, result) }

    def replyWith(all: List[MatchedPart]): Unit = {
      val (withCategories, withoutCategories) = all.partition(_.legoPart.exists(_.categories.nonEmpty))
      val prefixMatched = withoutCategories.map(mp => uploadLocalPrefixMatch(mp, withCategories))
      val sortedParts = (withCategories ++ prefixMatched).sorted
      logger.info(s"ProcessParts replying with ${sortedParts.size} output parts")
      replyTo ! ProcessedParts(sortedParts)
    }

    // image resolution applies to Modified results only; Guessed results keep their
    // synthesized identity (partNumber "", no image) and Misses have no lego part to enrich
    val modified = nonExact.filter { case (_, result) => result.via == TaxonomyHolder.Modified }

    if (modified.isEmpty) {
      replyWith(fallback)
    } else {
      implicit val rebrickableTimeout: Timeout = Timeout(30.seconds)
      val rebrickableDataFuture = rebrickableDataActor.ask(RebrickableHolder.GetData(_))
      rebrickableDataFuture.onComplete {
        case scala.util.Success(rebrickableData) =>
          val futures = modified.map { case (cp, result) =>
            finishModifiedPart(cp, result, rebrickableData, downloader, ldrawImageFetcher)
          }
          Future.sequence(futures).onComplete {
            case scala.util.Success(finished) =>
              val rest = nonExact
                .filter { case (_, result) => result.via != TaxonomyHolder.Modified }
                .map { case (cp, result) => buildMatchedPart(cp, result) }
              replyWith(exact.map { case (cp, result) => buildMatchedPart(cp, result) } ++ finished ++ rest)
            case scala.util.Failure(ex) =>
              logger.error(s"Failed to process parts: ${ex.getMessage}")
              replyWith(fallback)
          }

        case scala.util.Failure(ex) =>
          logger.error(s"Failed to get rebrickable data: ${ex.getMessage}")
          replyWith(fallback)
      }
    }
  }

  private def finishModifiedPart(
    coloredPart: ColoredPart,
    result: TaxonomyHolder.LookupResult,
    rebrickableData: com.wolfskeep.rebrickable.Data,
    downloader: ActorRef[CachedDownloader.Command],
    ldrawImageFetcher: LDrawImageFetcherTrait
  )(implicit ec: ExecutionContext, scheduler: akka.actor.typed.Scheduler, classicScheduler: akka.actor.Scheduler, logger: org.slf4j.Logger): Future[MatchedPart] = {
    val taxonomyPart = result.legoPart.get

    val colorNameToId = rebrickableData.colors.map(c => c.name -> c.id).toMap

    val ldImageUrl = Try(findPartImageUrl(coloredPart, colorNameToId, ldrawImageFetcher)).getOrElse(None)

    // hybrid: a long ask deadline (never races the base downloader's request
    // deadline) plus a short user-facing bound; when the UX bound wins, the
    // download continues in the background and is cached for later lookups
    val bricksetAskTimeout: Timeout = Timeout(90.seconds)
    val bricksetUxTimeout: FiniteDuration = 5.seconds

    val imageUrlFuture = ldImageUrl match {
      case Some(url) => Future.successful(Some(url))
      case None =>
        val bricksetFuture = Try(
          BricksetPartFetcher.fetchPartDetails(
            downloader,
            coloredPart.partNumber,
            coloredPart.elementId
          )(bricksetAskTimeout, scheduler, ec).map { bricksetResult =>
            bricksetResult.flatMap(_.imageUrl)
          }.recover { case _ => None }
        ).getOrElse(Future.successful(None))
        Future.firstCompletedOf(Seq(
          bricksetFuture,
          akka.pattern.after(bricksetUxTimeout, classicScheduler)(Future.successful(None))(ec)
        ))
    }

    imageUrlFuture.map { imageUrl =>
      MatchedPart(
        coloredPart,
        Some(taxonomyPart.copy(
          partNumber = coloredPart.partNumber,
          imageUrl = imageUrl,
          imageWidth = None,
          imageHeight = None
        )),
        result.categoriesGuessed
      )
    }
  }

  private def findPartImageUrl(
    coloredPart: ColoredPart,
    colorNameToId: Map[String, Int],
    ldrawImageFetcher: LDrawImageFetcherTrait
  )(implicit ec: ExecutionContext): Option[String] = {
    val colorIdOpt = colorNameToId.get(coloredPart.color)

    val rebrickableColorName = if (colorIdOpt.isEmpty) {
      StudioIoReader.colorMap.values.find { name =>
        name.equalsIgnoreCase(coloredPart.color)
      }
    } else {
      None
    }

    val finalColorId = colorIdOpt.orElse {
      rebrickableColorName.flatMap(name => colorNameToId.get(name))
    }

    finalColorId.flatMap { colorId =>
      val downloaded = ldrawImageFetcher.ensureDownloaded(colorId)
      if (!downloaded) {
        None
      } else {
        val hasImage = ldrawImageFetcher.hasImageInZip(colorId, coloredPart.partNumber)
        if (hasImage) {
          Some(s"part_images/$colorId/${coloredPart.partNumber}.png")
        } else {
          None
        }
      }
    }
  }

  private def uploadLocalPrefixMatch(mp: MatchedPart, candidates: List[MatchedPart]): MatchedPart = {
    val lowerName = mp.coloredPart.name.toLowerCase

    val prefixMatch = candidates
      .flatMap(_.legoPart)
      .flatMap { legoPart =>
        candidates.find(c => c.legoPart.contains(legoPart)).map { c => (legoPart, c.coloredPart.name) }
      }
      .filter { case (_, coloredPartName) =>
        lowerName.startsWith(coloredPartName.toLowerCase)
      }
      .sortBy { case (_, coloredPartName) => -coloredPartName.length }
      .headOption

    prefixMatch match {
      case Some((matchedLegoPart, _)) =>
        val newName = s"${matchedLegoPart.name} (guessed)"
        val updatedLegoPart = mp.legoPart.getOrElse(createGuessedLegoPart(newName, matchedLegoPart.categories))
          .copy(name = newName, categories = matchedLegoPart.categories)
        MatchedPart(mp.coloredPart, Some(updatedLegoPart), categoriesGuessed = true)
      case None =>
        mp
    }
  }

  private def createGuessedLegoPart(name: String, categories: List[Category]): LegoPart =
    LegoPart(
      partNumber = "",
      name = name,
      categories = categories,
      sequenceNumber = 0,
      altNumbers = Set.empty,
      imageUrl = None,
      imageWidth = None,
      imageHeight = None
    )
}
