package com.example

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors

import scala.concurrent.duration._
import akka.http.scaladsl.model.DateTime

object CachedDownloader {
  // public protocol
  sealed trait Command
  final case class Fetch(url: String, replyTo: ActorRef[Response]) extends Command

  sealed trait Response
  final case class Downloaded(url: String, content: String) extends Response
  final case class NotChanged(url: String) extends Response
  final case class Failed(url: String, reason: String) extends Response

  // internal messages
  private final case class CacheResponse(response: DiskCache.Response) extends Command
  private final case class DownloadResponse(response: Downloader.Response, oldValue: Option[String] = None) extends Command

  private case class State(pending: Map[String, List[ActorRef[Response]]])

  def apply(): Behavior[Command] = Behaviors.setup { context =>
    val downloader = context.spawn(Downloader(), "downloader")
    val cache = context.spawn(DiskCache(), "cache")

    running(State(Map.empty), downloader, cache)
  }

  private def running(state: State, downloader: ActorRef[Downloader.Command], cache: ActorRef[DiskCache.Command]): Behavior[Command] =
    Behaviors.receive[Command] { (context, message) =>
      message match {
        case Fetch(url, replyTo) =>
          state.pending.get(url) match {
            case Some(list) =>
              // already in progress, add to list
              val newList = list :+ replyTo
              val newPending = state.pending + (url -> newList)
              running(State(newPending), downloader, cache)
            case None =>
              // start new fetch
              val newPending = state.pending + (url -> List(replyTo))
              val cacheAdapter = context.messageAdapter[DiskCache.Response](resp => CacheResponse(resp))
              cache ! DiskCache.Fetch(url, cacheAdapter)
              running(State(newPending), downloader, cache)
          }

        case CacheResponse(cacheResp) =>
          cacheResp match {
            case DiskCache.FetchResult(key, value, time) =>
              val now = System.currentTimeMillis()
              val replyTos = state.pending(key)
              if (now - time < 3600000) {
                // fresh, reply directly
                replyTos.foreach(_ ! Downloaded(key, value))
                running(State(state.pending - key), downloader, cache)
              } else {
                // too old, do conditional download
                val since = Some(DateTime(time))
                val downloadAdapter = context.messageAdapter[Downloader.Response](resp => DownloadResponse(resp, Some(value)))
                downloader ! Downloader.Fetch(key, downloadAdapter, since)
                Behaviors.same
              }
            case DiskCache.NotFound(key) =>
              // no cache entry, just download
              val downloadAdapter = context.messageAdapter[Downloader.Response](resp => DownloadResponse(resp))
              downloader ! Downloader.Fetch(key, downloadAdapter, None)
              Behaviors.same
          }

        case DownloadResponse(downloadResp, oldValue) =>
          val url = downloadResp match {
            case Downloader.Downloaded(u, _) => u
            case Downloader.NotChanged(u) => u
            case Downloader.Failed(u, _) => u
          }
          val replyTos = state.pending(url)
          downloadResp match {
            case Downloader.Downloaded(_, content) =>
              // cache it
              cache ! DiskCache.Insert(url, content)
              replyTos.foreach(_ ! Downloaded(url, content))
            case Downloader.NotChanged(_) =>
              // update the timestamp in cache
              cache ! DiskCache.Insert(url, oldValue.getOrElse(""))
              replyTos.foreach(_ ! Downloaded(url, oldValue.getOrElse("")))
            case Downloader.Failed(_, reason) =>
              replyTos.foreach(_ ! Failed(url, reason))
          }
          running(State(state.pending - url), downloader, cache)
      
      }
    }.receiveSignal {
      case (_, _) => Behaviors.stopped
    }
}