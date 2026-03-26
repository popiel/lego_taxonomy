package com.wolfskeep

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.AskPattern._
import akka.util.Timeout

import scala.concurrent.duration._
import akka.http.scaladsl.model.DateTime
import akka.pattern.StatusReply.ErrorMessage
import scala.util.{Success, Failure}

object CachedDownloader {
  implicit val timeout: Timeout = 30.seconds
  private val StaleThresholdMs = 81000000L // 22.5 hours in milliseconds

  // public protocol
  sealed trait Command
  final case class Fetch(url: String, replyTo: ActorRef[Response]) extends Command

  sealed trait Response { def url: String }
  final case class Downloaded(url: String, content: String) extends Response
  final case class Failed(url: String, reason: Throwable) extends Response

  // internal messages
  private final case class CacheResult(url: String, result: Option[(String, String, Long)]) extends Command
  private final case class ForegroundFetchDone(url: String, response: Response) extends Command
  private final case class BackgroundRefreshDone(url: String) extends Command

  private case class PendingInfo(replyTos: List[ActorRef[Response]])
  private case class State(
    pending: Map[String, PendingInfo],     // Foreground fetches in progress
    refreshing: Set[String]                 // Background refreshes in progress
  )

  def apply(cache: ActorRef[DiskCache.Command], concurrencyLimit: Int = 10): Behavior[Command] = Behaviors.setup { context =>
    val baseDownloader = context.spawn(Downloader(retryOn429 = false), "downloader")
    val downloader = context.spawn(DownloadQueue(concurrencyLimit, baseDownloader), "queue")

    running(State(Map.empty, Set.empty), downloader, cache)
  }

  private def running(state: State, downloader: ActorRef[DownloadQueue.Command], cache: ActorRef[DiskCache.Command]): Behavior[Command] = {
    Behaviors.receive[Command] { (context, message) =>
      implicit val scheduler: akka.actor.typed.Scheduler = context.system.scheduler

      message match {
        case Fetch(url, replyTo) =>
          state.pending.get(url) match {
            case Some(info) =>
              // Foreground fetch already in progress - add caller to wait list
              val newInfo = info.copy(replyTos = info.replyTos :+ replyTo)
              running(State(state.pending + (url -> newInfo), state.refreshing), downloader, cache)

            case None =>
              // Check cache first
              context.ask(cache, (ref: ActorRef[DiskCache.Response]) => DiskCache.Fetch(url, ref)) {
                case Success(DiskCache.FetchResult(key, value, timestamp)) =>
                  CacheResult(url, Some((key, value, timestamp)))
                case _ =>
                  CacheResult(url, None)
              }
              // Add to pending optimistically
              running(State(state.pending + (url -> PendingInfo(List(replyTo))), state.refreshing), downloader, cache)
          }

        case CacheResult(url, Some((key, value, timestamp))) =>
          // Cache hit
          val now = System.currentTimeMillis()
          val age = now - timestamp

          val nextRefreshing = if (age >= StaleThresholdMs && !state.refreshing.contains(url)) {
            // Stale and not currently refreshing - trigger background refresh
            val since = DateTime(timestamp)
            startBackgroundRefresh(url, since, value, downloader, cache, context)
            state.refreshing + url
          } else {
            state.refreshing
          }

          // Return cached value to all callers
          val info = state.pending.getOrElse(url, PendingInfo(Nil))
          info.replyTos.foreach(_ ! Downloaded(key, value))

          running(State(state.pending - url, nextRefreshing), downloader, cache)

        case CacheResult(url, None) =>
          // Cache miss - need foreground fetch
          startForegroundFetch(url, None, None, downloader, cache, context)
          Behaviors.same

        case ForegroundFetchDone(url, response) =>
          // Foreground fetch complete - reply to all waiting callers
          val info = state.pending.getOrElse(url, PendingInfo(Nil))
          info.replyTos.foreach(_ ! response)
          running(State(state.pending - url, state.refreshing), downloader, cache)

        case BackgroundRefreshDone(url) =>
          // Background refresh complete - just remove from refreshing set
          running(State(state.pending, state.refreshing - url), downloader, cache)
      }
    }
  }

  private def startForegroundFetch(
    url: String,
    since: Option[DateTime],
    cachedValue: Option[String],
    downloader: ActorRef[DownloadQueue.Command],
    cache: ActorRef[DiskCache.Command],
    context: akka.actor.typed.scaladsl.ActorContext[Command]
  ): Unit = {
    context.ask(downloader, (ref: ActorRef[Downloader.Response]) => DownloadQueue.Fetch(url, ref, since)) {
      case Success(Downloader.Downloaded(_, content)) =>
        // New content downloaded - update cache and return to callers
        cache ! DiskCache.Insert(url, content)
        ForegroundFetchDone(url, Downloaded(url, content))

      case Success(Downloader.NotChanged(_)) =>
        // Server says cached value is still valid - update timestamp and return cached content
        cachedValue match {
          case Some(value) =>
            cache ! DiskCache.Insert(url, value)
            ForegroundFetchDone(url, Downloaded(url, value))
          case None =>
            // Shouldn't happen: NotChanged without If-Modified-Since (no cached value)
            // Treat as failure
            ForegroundFetchDone(url, Failed(url, ErrorMessage("Unexpected NotChanged without cached value")))
        }

      case Success(Downloader.Failed(_, reason)) =>
        // Download failed - return error to callers
        ForegroundFetchDone(url, Failed(url, ErrorMessage(reason)))

      case Success(Downloader.TooManyRequests(_)) =>
        // Rate limited - return error to callers (DownloadQueue handles retry)
        ForegroundFetchDone(url, Failed(url, ErrorMessage("HTTP 429 Too Many Requests")))

      case Failure(ex) =>
        // Ask timeout or other failure - return error to callers
        ForegroundFetchDone(url, Failed(url, ex))
    }
  }

  private def startBackgroundRefresh(
    url: String,
    since: DateTime,
    cachedValue: String,
    downloader: ActorRef[DownloadQueue.Command],
    cache: ActorRef[DiskCache.Command],
    context: akka.actor.typed.scaladsl.ActorContext[Command]
  ): Unit = {
    context.ask(downloader, (ref: ActorRef[Downloader.Response]) => DownloadQueue.Fetch(url, ref, Some(since))) {
      case Success(Downloader.Downloaded(_, content)) =>
        // New content downloaded - update cache
        cache ! DiskCache.Insert(url, content)
        BackgroundRefreshDone(url)

      case Success(Downloader.NotChanged(_)) =>
        // Server says cached value is still valid - update timestamp
        cache ! DiskCache.Insert(url, cachedValue)
        BackgroundRefreshDone(url)

      case _ =>
        // Refresh failed - cache remains stale but valid, caller already got stale value
        BackgroundRefreshDone(url)
    }
  }
}
