package com.wolfskeep

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}

object TaxonomyFetcher {
  sealed trait Command
  case class GetTaxonomy(replyTo: ActorRef[Response]) extends Command
  case class RegisterHolder(holder: ActorRef[TaxonomyHolder.Command]) extends Command

  sealed trait Response
  case object AugmentationComplete extends Response
  // communicated when the bulk category phase fails
  case class Failed(reason: Throwable) extends Response

  // internal (bi-directional tell; replies are correlated by URL)
  private final case class DownloadResponse(response: CachedDownloader.Response) extends Command

  private case class State(allCategories: Set[Category], allParts: List[LegoPart], pendingFetches: Int, partsToEnhance: List[LegoPart], replyTo: ActorRef[Response])

  def apply(): Behavior[Command] = Behaviors.setup { context =>
    val cache = context.spawn(DiskCache(), "cache")
    val downloader = context.spawn(CachedDownloader(cache), "downloader")
    val responseAdapter: ActorRef[CachedDownloader.Response] =
      context.messageAdapter[CachedDownloader.Response](DownloadResponse)

    idle(downloader, cache, None, responseAdapter)
  }

  // test seam: drive the state machine with an externally controlled downloader/cache
  def apply(downloader: ActorRef[CachedDownloader.Command], cache: ActorRef[DiskCache.Command]): Behavior[Command] =
    Behaviors.setup { context =>
      val responseAdapter: ActorRef[CachedDownloader.Response] =
        context.messageAdapter[CachedDownloader.Response](DownloadResponse)

      idle(downloader, cache, None, responseAdapter)
    }

  val rootUrl = "https://brickarchitect.com/parts/?&retired=1&partstyle=1"
  private val categoryUrlPrefix = "https://brickarchitect.com/parts/category-"
  private val partUrlPrefix = "https://brickarchitect.com/parts/"

  private def fetchUrl(downloader: ActorRef[CachedDownloader.Command], url: String, responseAdapter: ActorRef[CachedDownloader.Response]): Unit =
    downloader ! CachedDownloader.Fetch(url, responseAdapter)

  def idle(
    downloader: ActorRef[CachedDownloader.Command],
    cache: ActorRef[DiskCache.Command],
    holder: Option[ActorRef[TaxonomyHolder.Command]],
    responseAdapter: ActorRef[CachedDownloader.Response]
  ): Behavior[Command] = Behaviors.receive { (context, message) =>
    message match {
      case RegisterHolder(newHolder) =>
        idle(downloader, cache, Some(newHolder), responseAdapter)

      case GetTaxonomy(replyTo) =>
        // start the root fetch
        fetchUrl(downloader, rootUrl, responseAdapter)
        collecting(State(Set.empty, Nil, 1, Nil, replyTo), downloader, cache, holder, responseAdapter)

      case DownloadResponse(response) =>
        // late reply from an aborted cycle; expected with the tell-based protocol
        context.log.debug(s"Ignoring stray download response for ${response.url}")
        Behaviors.same

      case _ =>
        Behaviors.unhandled
    }
  }

  def collecting(
    state: State,
    downloader: ActorRef[CachedDownloader.Command],
    cache: ActorRef[DiskCache.Command],
    holder: Option[ActorRef[TaxonomyHolder.Command]],
    responseAdapter: ActorRef[CachedDownloader.Response]
  ): Behavior[Command] = Behaviors.receive { (context, message) =>
    message match {
      case RegisterHolder(newHolder) =>
        collecting(state, downloader, cache, Some(newHolder), responseAdapter)

      case DownloadResponse(CachedDownloader.Downloaded(url, content)) =>
        if (url == rootUrl) {
          val cats = TaxonomyParser.parseRootHtml(content)
          context.log.info(s"Fetched root with ${cats.size} categories")
          cats.foreach { cat =>
            fetchUrl(downloader, s"${categoryUrlPrefix}${cat.number}?&retired=1&partstyle=1", responseAdapter)
          }
          val newState = state.copy(pendingFetches = state.pendingFetches - 1 + cats.size)
          collecting(newState, downloader, cache, holder, responseAdapter)
        } else if (url.startsWith(categoryUrlPrefix)) {
          val (cats, parts) = TaxonomyParser.parseCategoryHtml(url, content)
          val newCats = state.allCategories ++ cats
          val newParts = state.allParts ++ parts
          val newPending = state.pendingFetches - 1

          context.log.info(s"Fetched category page $url with ${cats.size} categories, ${parts.size} parts")

          if (newPending == 0) {
            // bulk phase complete: publish the unaugmented taxonomy immediately
            val bulkData = TaxonomyData(newCats, newParts)
            context.log.info(s"Taxonomy published: ${newCats.size} categories, ${newParts.size} parts")
            holder.foreach(_ ! TaxonomyHolder.SetTaxonomy(bulkData))

            context.log.info(s"Starting to enhance ${newParts.size} parts")
            val (initialBatch, remaining) = newParts.splitAt(20)
            initialBatch.foreach { part =>
              fetchUrl(downloader, s"$partUrlPrefix${part.partNumber}?&retired=1&partstyle=1", responseAdapter)
            }
            val enhanceState = State(newCats, newParts, initialBatch.size, remaining, state.replyTo)
            enhanceParts(enhanceState, downloader, cache, holder, responseAdapter)
          } else {
            collecting(State(newCats, newParts, newPending, Nil, state.replyTo), downloader, cache, holder, responseAdapter)
          }
        } else {
          collecting(state, downloader, cache, holder, responseAdapter)
        }

      case DownloadResponse(CachedDownloader.Failed(_, reason)) =>
        // propagate failure immediately and abandon further work
        context.log.error(s"download failed: ${reason}")
        state.replyTo ! Failed(reason)
        idle(downloader, cache, holder, responseAdapter)

      case GetTaxonomy(_) =>
        // ignore additional requests while already collecting
        Behaviors.unhandled
    }
  }

  def enhanceParts(
    state: State,
    downloader: ActorRef[CachedDownloader.Command],
    cache: ActorRef[DiskCache.Command],
    holder: Option[ActorRef[TaxonomyHolder.Command]],
    responseAdapter: ActorRef[CachedDownloader.Response]
  ): Behavior[Command] = Behaviors.receive { (context, message) =>
    message match {
      case RegisterHolder(newHolder) =>
        enhanceParts(state, downloader, cache, Some(newHolder), responseAdapter)

      case DownloadResponse(CachedDownloader.Downloaded(url, content)) =>
        val partNum = url.substring(partUrlPrefix.length).split("\\?")(0)
        val altNumbers = TaxonomyParser.parseAltNumbers(content, partNum)
        holder.foreach(_ ! TaxonomyHolder.AugmentPart(partNum, altNumbers))
        advanceWindow(context, state, downloader, cache, holder, responseAdapter)

      case DownloadResponse(CachedDownloader.Failed(_, reason)) =>
        // augmentation failures only lose that part's alt numbers; the taxonomy stays intact
        context.log.warn(s"part page fetch failed, skipping augmentation: ${reason}")
        advanceWindow(context, state, downloader, cache, holder, responseAdapter)

      case GetTaxonomy(_) =>
        Behaviors.unhandled
    }
  }

  private def advanceWindow(
    context: ActorContext[Command],
    state: State,
    downloader: ActorRef[CachedDownloader.Command],
    cache: ActorRef[DiskCache.Command],
    holder: Option[ActorRef[TaxonomyHolder.Command]],
    responseAdapter: ActorRef[CachedDownloader.Response]
  ): Behavior[Command] = {
    val newPending = state.pendingFetches - 1

    val (nextToFetch, remaining) = if (state.partsToEnhance.nonEmpty && newPending < 20) {
      state.partsToEnhance.splitAt(1)
    } else {
      (Nil, state.partsToEnhance)
    }

    nextToFetch.foreach { part =>
      fetchUrl(downloader, s"$partUrlPrefix${part.partNumber}?&retired=1&partstyle=1", responseAdapter)
    }

    val newPendingWithNext = newPending + nextToFetch.size
    val totalRemaining = remaining.size + newPendingWithNext

    val previousRemaining = state.partsToEnhance.size + state.pendingFetches
    val prevThreshold = previousRemaining / 100
    val newThreshold = totalRemaining / 100
    if (prevThreshold != newThreshold) {
      context.log.info(s"Parts remaining to enhance: $totalRemaining")
    }

    if (newPendingWithNext == 0 && remaining.isEmpty) {
      context.log.info(s"All parts enhanced, completing")
      state.replyTo ! AugmentationComplete
      idle(downloader, cache, holder, responseAdapter)
    } else {
      enhanceParts(State(state.allCategories, state.allParts, newPendingWithNext, remaining, state.replyTo), downloader, cache, holder, responseAdapter)
    }
  }
}
