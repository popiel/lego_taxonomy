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
  private final case class CacheResponse(response: CacheActor.Response, url: String) extends Command
  private final case class DownloadResponse(response: Downloader.Response, url: String) extends Command
  private final case class CacheValueResponse(response: CacheActor.Response, url: String, replyTos: List[ActorRef[Response]]) extends Command

  private case class State(pending: Map[String, List[ActorRef[Response]]])

  def apply(): Behavior[Command] = Behaviors.setup { context =>
    val downloader = context.spawn(Downloader(), "downloader")
    val cache = context.spawn(CacheActor(), "cache")

    running(State(Map.empty), downloader, cache)
  }

  private def running(state: State, downloader: ActorRef[Downloader.Command], cache: ActorRef[CacheActor.Command]): Behavior[Command] =
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
              val cacheAdapter = context.messageAdapter[CacheActor.Response](resp => CacheResponse(resp, url))
              cache ! CacheActor.GetInsertionTime(url, cacheAdapter)
              running(State(newPending), downloader, cache)
          }

        case CacheResponse(cacheResp, url) =>
          val since = cacheResp match {
            case CacheActor.InsertionTime(time) =>
              Some(DateTime(time))
            case _ => None
          }
          val downloadAdapter = context.messageAdapter[Downloader.Response](resp => DownloadResponse(resp, url))
          downloader ! Downloader.Fetch(url, downloadAdapter, since)
          Behaviors.same

        case DownloadResponse(downloadResp, url) =>
          val replyTos = state.pending(url)
          downloadResp match {
            case Downloader.Downloaded(_, content) =>
              // cache it
              cache ! CacheActor.Insert(url, content)
              replyTos.foreach(_ ! Downloaded(url, content))
            case Downloader.NotChanged(_) =>
              // get the cached value
              val valueAdapter = context.messageAdapter[CacheActor.Response](resp => CacheValueResponse(resp, url, replyTos))
              cache ! CacheActor.GetValue(url, valueAdapter)
            case Downloader.Failed(_, reason) =>
              replyTos.foreach(_ ! Failed(url, reason))
          }
          val newPending = state.pending - url
          running(State(newPending), downloader, cache)

        case CacheValueResponse(cacheResp, url, replyTos) =>
          cacheResp match {
            case CacheActor.Value(content) =>
              replyTos.foreach(_ ! Downloaded(url, content))
            case _ =>
              // should not happen, but reply failed
              replyTos.foreach(_ ! Failed(url, "Cache inconsistency"))
          }
          Behaviors.same
      }
    }.receiveSignal {
      case (_, _) => Behaviors.stopped
    }
}