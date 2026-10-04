package com.wolfskeep

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.model.DateTime

object CachedDownloader {
  private val StaleThresholdMs = 81000000L // 22.5 hours in milliseconds

  // public protocol
  sealed trait Command
  final case class Fetch(url: String, replyTo: ActorRef[Response]) extends Command

  sealed trait Response { def url: String }
  final case class Downloaded(url: String, content: String) extends Response
  final case class Failed(url: String, reason: Throwable) extends Response

  // internal messages (bi-directional tells; replies are correlated by URL)
  private final case class CacheLookup(response: DiskCache.Response) extends Command
  private final case class QueueReply(response: Downloader.Response) extends Command

  private case class State(
    pending: Map[String, List[ActorRef[Response]]],       // URL -> callers waiting for a reply
    foreground: Set[String],                             // URLs fetched because the cache missed
    refreshing: Map[String, (String, DateTime)]           // URL -> (cached value, If-Modified-Since) for background refresh
  )

  def apply(cache: ActorRef[DiskCache.Command], concurrencyLimit: Int = 10): Behavior[Command] = Behaviors.setup { context =>
    val baseDownloader = context.spawn(Downloader(retryOn429 = false), "downloader")
    val downloader = context.spawn(DownloadQueue(concurrencyLimit, baseDownloader), "queue")
    val cacheAdapter: ActorRef[DiskCache.Response] = context.messageAdapter[DiskCache.Response](CacheLookup)
    val queueAdapter: ActorRef[Downloader.Response] = context.messageAdapter[Downloader.Response](QueueReply)

    running(State(Map.empty, Set.empty, Map.empty), downloader, cache, cacheAdapter, queueAdapter)
  }

  private def running(
    state: State,
    downloader: ActorRef[DownloadQueue.Command],
    cache: ActorRef[DiskCache.Command],
    cacheAdapter: ActorRef[DiskCache.Response],
    queueAdapter: ActorRef[Downloader.Response]
  ): Behavior[Command] = {
    Behaviors.receive[Command] { (context, message) =>
      def replyPending(url: String, response: Response): Unit =
        state.pending.getOrElse(url, Nil).foreach(_ ! response)

      def without(url: String): State =
        State(state.pending - url, state.foreground - url, state.refreshing - url)

      message match {
        case Fetch(url, replyTo) =>
          val newPending = state.pending.get(url) match {
            case Some(replyTos) =>
              state.pending + (url -> (replyTo :: replyTos))
            case None =>
              cache ! DiskCache.Fetch(url, cacheAdapter)
              state.pending + (url -> List(replyTo))
          }
          running(State(newPending, state.foreground, state.refreshing), downloader, cache, cacheAdapter, queueAdapter)

        case CacheLookup(DiskCache.FetchResult(url, value, timestamp)) =>
          val now = System.currentTimeMillis()
          val age = now - timestamp

          val nextRefreshing = if (age >= StaleThresholdMs && !state.refreshing.contains(url)) {
            val since = DateTime(timestamp)
            downloader ! DownloadQueue.Fetch(url, queueAdapter, Some(since))
            state.refreshing + (url -> (value, since))
          } else {
            state.refreshing
          }

          replyPending(url, Downloaded(url, value))
          running(State(state.pending - url, state.foreground, nextRefreshing), downloader, cache, cacheAdapter, queueAdapter)

        case CacheLookup(DiskCache.NotFound(url)) =>
          downloader ! DownloadQueue.Fetch(url, queueAdapter, None)
          running(State(state.pending, state.foreground + url, state.refreshing), downloader, cache, cacheAdapter, queueAdapter)

        case QueueReply(Downloader.Downloaded(url, content)) =>
          cache ! DiskCache.Insert(url, content)
          replyPending(url, Downloaded(url, content))
          running(without(url), downloader, cache, cacheAdapter, queueAdapter)

        case QueueReply(Downloader.NotChanged(url)) =>
          state.refreshing.get(url) match {
            case Some((value, _)) =>
              cache ! DiskCache.Insert(url, value)
            case None =>
              // foreground fetches are always cold (no If-Modified-Since)
              context.log.warn(s"Ignoring stray NotChanged for $url")
          }
          running(without(url), downloader, cache, cacheAdapter, queueAdapter)

        case QueueReply(Downloader.Failed(url, reason)) =>
          if (state.foreground.contains(url)) {
            replyPending(url, Failed(url, new RuntimeException(reason)))
          } else if (state.refreshing.contains(url)) {
            context.log.warn(s"background refresh failed for $url: $reason")
          } else {
            context.log.warn(s"Ignoring stray downloader failure for $url")
          }
          running(without(url), downloader, cache, cacheAdapter, queueAdapter)

        case QueueReply(Downloader.TooManyRequests(url)) =>
          // the queue pauses on 429 rather than forwarding; reaching here is unexpected
          replyPending(url, Failed(url, new RuntimeException("HTTP 429 Too Many Requests")))
          running(without(url), downloader, cache, cacheAdapter, queueAdapter)
      }
    }
  }
}
