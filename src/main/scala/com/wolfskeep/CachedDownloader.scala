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
  sealed trait Command { def url: String }
  final case class Fetch(url: String, replyTo: ActorRef[Response]) extends Command

  sealed trait Response { def url: String }
  final case class Downloaded(url: String, content: String) extends Response
  final case class Failed(url: String, reason: Throwable) extends Response

  // internal messages
  private final case class CacheResult(url: String, result: Option[(String, Long)]) extends Command
  private final case class ForegroundFetchDone(response: Response) extends Command {
    def url = response.url
  }
  private final case class BackgroundRefreshDone(url: String) extends Command

  private case class State(
    pending: Map[String, List[ActorRef[Response]]],  // URL -> callers waiting for foreground fetch
    refreshing: Set[String]                            // URLs being background refreshed
  )

  def apply(cache: ActorRef[DiskCache.Command], concurrencyLimit: Int = 10): Behavior[Command] = Behaviors.setup { context =>
    val baseDownloader = context.spawn(Downloader(retryOn429 = false), "downloader")
    val downloader = context.spawn(DownloadQueue(concurrencyLimit, baseDownloader), "queue")

    running(State(Map.empty, Set.empty), downloader, cache)
  }

  private def running(state: State, downloader: ActorRef[DownloadQueue.Command], cache: ActorRef[DiskCache.Command]): Behavior[Command] = {
    Behaviors.receive[Command] { (context, message) =>
      def startForegroundFetch(since: Option[DateTime], cachedValue: Option[String]): Unit = {
        val url = message.url
        context.ask(downloader, (ref: ActorRef[Downloader.Response]) => DownloadQueue.Fetch(url, ref, since)) {
          case Success(Downloader.Downloaded(_, content)) =>
            cache ! DiskCache.Insert(url, content)
            ForegroundFetchDone(Downloaded(url, content))

          case Success(Downloader.NotChanged(_)) =>
            cachedValue match {
              case Some(value) =>
                cache ! DiskCache.Insert(url, value)
                ForegroundFetchDone(Downloaded(url, value))
              case None =>
                ForegroundFetchDone(Failed(url, ErrorMessage("Unexpected NotChanged without cached value")))
            }

          case Success(Downloader.Failed(_, reason)) =>
            ForegroundFetchDone(Failed(url, ErrorMessage(reason)))

          case Success(Downloader.TooManyRequests(_)) =>
            ForegroundFetchDone(Failed(url, ErrorMessage("HTTP 429 Too Many Requests")))

          case Failure(ex) =>
            ForegroundFetchDone(Failed(url, ex))
        }
      }

      def startBackgroundRefresh(since: DateTime, cachedValue: String): Unit = {
        val url = message.url
        context.ask(downloader, (ref: ActorRef[Downloader.Response]) => DownloadQueue.Fetch(url, ref, Some(since))) {
          case Success(Downloader.Downloaded(_, content)) =>
            cache ! DiskCache.Insert(url, content)
            BackgroundRefreshDone(url)

          case Success(Downloader.NotChanged(_)) =>
            cache ! DiskCache.Insert(url, cachedValue)
            BackgroundRefreshDone(url)

          case _ =>
            BackgroundRefreshDone(url)
        }
      }

      message match {
        case Fetch(_, replyTo) =>
          state.pending.get(message.url) match {
            case Some(replyTos) =>
              running(State(state.pending + (message.url -> (replyTo :: replyTos)), state.refreshing), downloader, cache)

            case None =>
              context.ask(cache, (ref: ActorRef[DiskCache.Response]) => DiskCache.Fetch(message.url, ref)) {
                case Success(DiskCache.FetchResult(_, value, timestamp)) =>
                  CacheResult(message.url, Some((value, timestamp)))
                case _ =>
                  CacheResult(message.url, None)
              }
              running(State(state.pending + (message.url -> List(replyTo)), state.refreshing), downloader, cache)
          }

        case CacheResult(_, Some((value, timestamp))) =>
          val url = message.url
          val now = System.currentTimeMillis()
          val age = now - timestamp

          val nextRefreshing = if (age >= StaleThresholdMs && !state.refreshing.contains(url)) {
            startBackgroundRefresh(DateTime(timestamp), value)
            state.refreshing + url
          } else {
            state.refreshing
          }

          state.pending.getOrElse(url, Nil).foreach(_ ! Downloaded(url, value))
          running(State(state.pending - url, nextRefreshing), downloader, cache)

        case CacheResult(_, None) =>
          startForegroundFetch(None, None)
          Behaviors.same

        case ForegroundFetchDone(response) =>
          state.pending.getOrElse(message.url, Nil).foreach(_ ! response)
          running(State(state.pending - message.url, state.refreshing), downloader, cache)

        case BackgroundRefreshDone(_) =>
          running(State(state.pending, state.refreshing - message.url), downloader, cache)
      }
    }
  }
}
