package com.example

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import scala.concurrent.duration._
import akka.util.Timeout

object TaxonomyFetcher {
  // reuse same timeout as the downloader for ask operations
  implicit val timeout: Timeout = CachedDownloader.timeout
  sealed trait Command
  case class GetTaxonomy(replyTo: ActorRef[Response]) extends Command

  sealed trait Response
  case class TaxonomyFetched(categories: Set[Category], parts: List[LegoPart]) extends Response
  // communicated when any fetch or ask fails
  case class Failed(reason: Throwable) extends Response

  // internal
  private case class FetchResponse(response: CachedDownloader.Response) extends Command
  private case class AskFailure(ex: Throwable) extends Command

  private case class State(allCategories: Set[Category], allParts: List[LegoPart], pendingFetches: Int, replyTo: ActorRef[Response], remainingParts: List[LegoPart] = Nil)

  def apply(): Behavior[Command] = Behaviors.setup { context =>
    implicit val timeout: Timeout = CachedDownloader.timeout
    val cache = context.spawn(DiskCache(), "cache")
    val downloader = context.spawn(CachedDownloader(cache), "downloader")

    idle(downloader, cache)
  }

  val rootUrl = "https://brickarchitect.com/parts/?&retired=1&partstyle=1"

  def idle(downloader: ActorRef[CachedDownloader.Command], cache: ActorRef[DiskCache.Command]): Behavior[Command] = Behaviors.receive { (context, message) =>
    message match {
      case GetTaxonomy(replyTo) =>
        // start root fetch using ask to handle potential failures
        context.ask(downloader, (ref: ActorRef[CachedDownloader.Response]) =>
          CachedDownloader.Fetch(rootUrl, ref)
        ) {
          case scala.util.Success(resp) => FetchResponse(resp)
          case scala.util.Failure(ex)     => AskFailure(ex)
        }
        collecting(State(Set.empty, Nil, 1, replyTo), downloader, cache)
      case _ =>
        Behaviors.unhandled
    }
  }

  def collecting(state: State, downloader: ActorRef[CachedDownloader.Command], cache: ActorRef[DiskCache.Command]): Behavior[Command] = Behaviors.receive { (context, message) =>
    message match {
      case FetchResponse(CachedDownloader.Downloaded(url, content)) =>
        if (url == rootUrl) {
          val cats = TaxonomyParser.parseRootHtml(content)
          context.log.info(s"Fetched root with ${cats.size} categories")
          cats.foreach { cat =>
            val catUrl = s"https://brickarchitect.com/parts/category-${cat.number}?&retired=1&partstyle=1"
            // use ask for subsequent fetches as well
            context.ask(downloader, (ref: ActorRef[CachedDownloader.Response]) =>
              CachedDownloader.Fetch(catUrl, ref)
            ) {
              case scala.util.Success(resp) => FetchResponse(resp)
              case scala.util.Failure(ex)     => AskFailure(ex)
            }
          }
          val newState = state.copy(pendingFetches = state.pendingFetches - 1 + cats.size)
          collecting(newState, downloader, cache)
        } else if (url.startsWith("https://brickarchitect.com/parts/category-")) {
          val (cats, parts) = TaxonomyParser.parseCategoryHtml(url, content)
          val newCats = state.allCategories ++ cats
          val newParts = state.allParts ++ parts
          val newPending = state.pendingFetches - 1

          context.log.info(s"Fetched category page $url with ${cats.size} categories, ${parts.size} parts")

          val newState = State(newCats, newParts, newPending, state.replyTo)
          if (newPending == 0) {
            context.log.info(s"Starting to enhance ${newParts.size} parts")
            val batch = newParts.take(10)
            val remaining = newParts.drop(10)
            batch.foreach { part =>
              val partUrl = s"https://brickarchitect.com/parts/${part.partNumber}?&retired=1&partstyle=1"
              context.ask(downloader, (ref: ActorRef[CachedDownloader.Response]) =>
                CachedDownloader.Fetch(partUrl, ref)
              ) {
                case scala.util.Success(resp) => FetchResponse(resp)
                case scala.util.Failure(ex)     => AskFailure(ex)
              }
            }
            val enhanceState = State(newCats, newParts, batch.size, state.replyTo, remaining)
            enhanceParts(enhanceState, downloader, cache)
          } else {
            collecting(newState, downloader, cache)
          }
        } else {
          collecting(state, downloader, cache)
        }

      case FetchResponse(CachedDownloader.Failed(_, reason)) =>
        // propagate failure immediately and abandon further work
        context.log.error(s"download failed: ${reason}")
        state.replyTo ! Failed(reason)
        idle(downloader, cache)

      case AskFailure(ex) =>
        context.log.error("ask to downloader failed", ex)
        state.replyTo ! Failed(ex)
        idle(downloader, cache)
      case GetTaxonomy(_) =>
        // ignore additional requests while already collecting
        Behaviors.unhandled
    }
  }

  def enhanceParts(state: State, downloader: ActorRef[CachedDownloader.Command], cache: ActorRef[DiskCache.Command]): Behavior[Command] = Behaviors.receive { (context, message) =>
    message match {
      case FetchResponse(CachedDownloader.Downloaded(url, content)) =>
        val partNum = url.substring("https://brickarchitect.com/parts/".length).split("\\?")(0)
        val enhancedParts = state.allParts.map { part =>
          if (part.partNumber == partNum) {
            TaxonomyParser.enhancePart(part, content)
          } else {
            part
          }
        }
        val newPending = state.pendingFetches - 1
        // context.log.info(s"Enhanced part $partNum, ${newPending} parts remaining")

        if (newPending == 0) {
          if (state.remainingParts.isEmpty) {
            context.log.info(s"All parts enhanced, completing")
            state.replyTo ! TaxonomyFetched(state.allCategories, enhancedParts)
            idle(downloader, cache)
          } else {
            context.log.info(s"Starting next batch, ${state.remainingParts.size} parts remaining")
            val batch = state.remainingParts.take(10)
            val remaining = state.remainingParts.drop(10)
            batch.foreach { part =>
              val partUrl = s"https://brickarchitect.com/parts/${part.partNumber}?&retired=1&partstyle=1"
              context.ask(downloader, (ref: ActorRef[CachedDownloader.Response]) =>
                CachedDownloader.Fetch(partUrl, ref)
              ) {
                case scala.util.Success(resp) => FetchResponse(resp)
                case scala.util.Failure(ex)     => AskFailure(ex)
              }
            }
            val newState = State(state.allCategories, enhancedParts, batch.size, state.replyTo, remaining)
            enhanceParts(newState, downloader, cache)
          }
        } else {
          val newState = State(state.allCategories, enhancedParts, newPending, state.replyTo, state.remainingParts)
          enhanceParts(newState, downloader, cache)
        }

      case FetchResponse(CachedDownloader.Failed(_, reason)) =>
        context.log.error(s"download failed: ${reason}")
        state.replyTo ! Failed(reason)
        idle(downloader, cache)

      case AskFailure(ex) =>
        context.log.error("ask to downloader failed", ex)
        state.replyTo ! Failed(ex)
        idle(downloader, cache)

      case GetTaxonomy(_) =>
        Behaviors.unhandled
    }
  }
}
