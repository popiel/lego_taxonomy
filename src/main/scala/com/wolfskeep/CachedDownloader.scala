package com.wolfskeep

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.AskPattern._
import akka.util.Timeout

import scala.concurrent.duration._
import akka.http.scaladsl.model.DateTime
import akka.pattern.StatusReply.ErrorMessage
import scala.concurrent.Future

object CachedDownloader {
  implicit val timeout: Timeout = 30.seconds

  // public protocol
  sealed trait Command
  final case class Fetch(url: String, replyTo: ActorRef[Response]) extends Command

  sealed trait Response { def url: String }
  final case class Downloaded(url: String, content: String) extends Response
  final case class Failed(url: String, reason: Throwable) extends Response

  // internal messages
  private final case class ReplyWith(response: Response) extends Command

  private case class PendingInfo(replyTos: List[ActorRef[Response]], oldValue: String)
  private case class State(pending: Map[String, PendingInfo])

  def apply(cache: ActorRef[DiskCache.Command]): Behavior[Command] = Behaviors.setup { context =>
    val downloader = context.spawn(Downloader(), "downloader")

    running(State(Map.empty), downloader, cache)
  }

  private def running(state: State, downloader: ActorRef[Downloader.Command], cache: ActorRef[DiskCache.Command]): Behavior[Command] = {
    val behavior = Behaviors.receive[Command] { (context, message) =>
      implicit val scheduler = context.system.scheduler
      import context.executionContext
      message match {
        case Fetch(url, replyTo) =>
          state.pending.get(url) match {
            case Some(info) =>
              // already in progress, add to list
              val newInfo = info.copy(replyTos = info.replyTos :+ replyTo)
              running(State(state.pending + (url -> newInfo)), downloader, cache)
            case None =>
              // start new fetch
              def networkFetch(since: Option[DateTime], value: Option[String]): Future[Response] = {
                downloader.ask(Downloader.Fetch(url, _, since)).map {
                  case Downloader.Downloaded(key, content) =>
                    cache ! DiskCache.Insert(key, content)
                    Downloaded(key, content)
                  case Downloader.NotChanged(key) =>
                    cache ! DiskCache.Insert(key, value.get)
                    Downloaded(key, value.get)
                  case Downloader.Failed(key, reason) =>
                    Failed(key, ErrorMessage(reason))
                  }
              }

              context.pipeToSelf(
                cache.ask(DiskCache.Fetch(url, _)).recover {
                  case ex => DiskCache.NotFound(url)
                }.flatMap {
                  case DiskCache.FetchResult(key, value, time) =>
                    val now = System.currentTimeMillis()
                    if (now - time < 81000000) { // 22.5 hours
                      Future.successful(Downloaded(key, value))
                    } else {
                      networkFetch(Some(DateTime(time)), Some(value))
                    }
                  case DiskCache.NotFound(key) => networkFetch(None, None)
                }
              ) {
                case scala.util.Success(resp) => ReplyWith(resp)
                case scala.util.Failure(ex) => ReplyWith(Failed(url, ex))
              }
              val newInfo = PendingInfo(List(replyTo), "")
              running(State(state.pending + (url -> newInfo)), downloader, cache)
          }

        case ReplyWith(response) =>
          val info = state.pending(response.url)
          info.replyTos.foreach(_ ! response)
          running(State(state.pending - response.url), downloader, cache)
      }
    }

    behavior.receiveSignal {
      case (_, _) => Behaviors.stopped
    }
  }
}