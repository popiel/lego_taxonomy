package com.example

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import akka.util.Timeout

import scala.concurrent.duration._
import akka.http.scaladsl.model.DateTime
import akka.pattern.StatusReply.ErrorMessage

object CachedDownloader {
  implicit val timeout: Timeout = 30.seconds

  // public protocol
  sealed trait Command
  final case class Fetch(url: String, replyTo: ActorRef[Response]) extends Command

  sealed trait Response
  final case class Downloaded(url: String, content: String) extends Response
  final case class Failed(url: String, reason: Throwable) extends Response

  // internal messages
  private final case class CacheResponse(response: DiskCache.Response) extends Command
  private final case class DownloadResponse(response: Downloader.Response) extends Command
  private case class DownloadFailed(url: String, ex: Throwable) extends Command
  private case class CacheFailed(url: String, ex: Throwable) extends Command

  private case class PendingInfo(replyTos: List[ActorRef[Response]], oldValue: String)
  private case class State(pending: Map[String, PendingInfo])

  def apply(): Behavior[Command] = Behaviors.setup { context =>
    val downloader = context.spawn(Downloader(), "downloader")
    val cache = context.spawn(DiskCache(), "cache")

    running(State(Map.empty), downloader, cache)
  }

  private def running(state: State, downloader: ActorRef[Downloader.Command], cache: ActorRef[DiskCache.Command]): Behavior[Command] = {
    val behavior = Behaviors.receive[Command] { (context, message) =>
      message match {
        case Fetch(url, replyTo) =>
          state.pending.get(url) match {
            case Some(info) =>
              // already in progress, add to list
              val newInfo = info.copy(replyTos = info.replyTos :+ replyTo)
              running(State(state.pending + (url -> newInfo)), downloader, cache)
            case None =>
              // start new fetch
              val newInfo = PendingInfo(List(replyTo), "")
              context.ask(cache, DiskCache.Fetch(url, _)) {
                case scala.util.Success(resp) => CacheResponse(resp)
                case scala.util.Failure(ex) => CacheFailed(url, ex)
              }
              running(State(state.pending + (url -> newInfo)), downloader, cache)
          }

        case CacheResponse(cacheResp) =>
          cacheResp match {
            case DiskCache.FetchResult(key, value, time) =>
              val now = System.currentTimeMillis()
              val info = state.pending(key)
              if (now - time < 3600000) {
                // fresh, reply directly
                info.replyTos.foreach(_ ! Downloaded(key, value))
                running(State(state.pending - key), downloader, cache)
              } else {
                // too old, do conditional download
                val since = Some(DateTime(time))
                val updatedInfo = info.copy(oldValue = value)
                context.ask(downloader, Downloader.Fetch(key, _, since)) {
                  case scala.util.Success(resp) => DownloadResponse(resp)
                  case scala.util.Failure(ex) => DownloadFailed(key, ex)
                }
                running(State(state.pending + (key -> updatedInfo)), downloader, cache)
              }
            case DiskCache.NotFound(key) =>
              // no cache entry, just download
              val info = state.pending(key)
              val updatedInfo = info.copy(oldValue = "") // already ""
              context.ask(downloader, Downloader.Fetch(key, _, None)) {
                case scala.util.Success(resp) => DownloadResponse(resp)
                case scala.util.Failure(ex) => DownloadFailed(key, ex)
              }
              running(State(state.pending + (key -> updatedInfo)), downloader, cache)
          }

        case CacheFailed(url, ex) =>
          val info = state.pending(url)
          info.replyTos.foreach(_ ! Failed(url, ex))
          running(State(state.pending - url), downloader, cache)

        case DownloadResponse(Downloader.Downloaded(url, content)) =>
          val info = state.pending(url)
          // cache it
          cache ! DiskCache.Insert(url, content)
          info.replyTos.foreach(_ ! Downloaded(url, content))
          running(State(state.pending - url), downloader, cache)

        case DownloadResponse(Downloader.NotChanged(url)) =>
          val info = state.pending(url)
          // update the timestamp in cache
          cache ! DiskCache.Insert(url, info.oldValue)
          info.replyTos.foreach(_ ! Downloaded(url, info.oldValue))
          running(State(state.pending - url), downloader, cache)

        case DownloadResponse(Downloader.Failed(url, reason)) =>
          val info = state.pending(url)
          info.replyTos.foreach(_ ! Failed(url, ErrorMessage(reason)))
          running(State(state.pending - url), downloader, cache)

        case DownloadFailed(url, ex) =>
          val info = state.pending(url)
          info.replyTos.foreach(_ ! Failed(url, ex))
          running(State(state.pending - url), downloader, cache)
      }
    }

    behavior.receiveSignal {
      case (_, _) => Behaviors.stopped
    }
  }
}