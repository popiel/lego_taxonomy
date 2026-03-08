package com.example

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors

object TaxonomyFetcher {
  sealed trait Command
  case class GetTaxonomy(replyTo: ActorRef[Response]) extends Command

  sealed trait Response
  case class TaxonomyFetched(categories: Set[Category], parts: List[LegoPart]) extends Response

  // internal
  private case class FetchResponse(url: String, response: CachedDownloader.Response) extends Command

  private case class State(allCategories: Set[Category], allParts: List[LegoPart], pendingFetches: Int, replyTo: ActorRef[Response])

  def apply(): Behavior[Command] = Behaviors.setup { context =>
    val cache = context.spawn(DiskCache(), "cache")
    val downloader = context.spawn(CachedDownloader(cache), "downloader")

    idle(downloader, cache)
  }

  def idle(downloader: ActorRef[CachedDownloader.Command], cache: ActorRef[DiskCache.Command]): Behavior[Command] = Behaviors.receive { (context, message) =>
    message match {
      case GetTaxonomy(replyTo) =>
        val replyToAdapter = context.messageAdapter[CachedDownloader.Response](resp => FetchResponse("root", resp))
        downloader ! CachedDownloader.Fetch("https://brickarchitect.com/parts/?&retired=1&partstyle=1", replyToAdapter)
        collecting(State(Set.empty, Nil, 1, replyTo), downloader, cache)
    }
  }

  def collecting(state: State, downloader: ActorRef[CachedDownloader.Command], cache: ActorRef[DiskCache.Command]): Behavior[Command] = Behaviors.receive { (context, message) =>
    message match {
      case FetchResponse(url, CachedDownloader.Downloaded(_, content)) =>
        val (cats, parts) = if (url == "root") (HtmlParser.parseRootHtml(content), Nil) else HtmlParser.parseCategoryHtml(url, content)
        val newCats = state.allCategories ++ cats
        val newParts = state.allParts ++ parts
        val newPending = if (url == "root") state.pendingFetches - 1 + cats.size else state.pendingFetches - 1

        if (url == "root") {
          cats.foreach { cat =>
            val catUrl = s"https://brickarchitect.com/parts/category-${cat.number}?&retired=1&partstyle=1"
            val replyToAdapter = context.messageAdapter[CachedDownloader.Response](resp => FetchResponse(catUrl, resp))
            downloader ! CachedDownloader.Fetch(catUrl, replyToAdapter)
          }
        }

        val newState = State(newCats, newParts, newPending, state.replyTo)
        if (newPending == 0) {
          state.replyTo ! TaxonomyFetched(newCats, newParts)
          idle(downloader, cache)
        } else {
          collecting(newState, downloader, cache)
        }

      case FetchResponse(url, CachedDownloader.Failed(_, reason)) =>
        context.log.error(s"Failed to fetch $url: $reason")
        val newPending = state.pendingFetches - 1
        if (newPending == 0) {
          state.replyTo ! TaxonomyFetched(state.allCategories, state.allParts)
          idle(downloader, cache)
        } else {
          collecting(State(state.allCategories, state.allParts, newPending, state.replyTo), downloader, cache)
        }
    }
  }

  def getParentChain(cat: Category): List[String] = cat.number :: cat.parent.map(getParentChain).getOrElse(Nil)

  def escapeCsv(s: String): String = if (s.contains(",") || s.contains("\"") || s.contains("\n")) s""""${s.replace("\"", "\"\"")}""" else s
}