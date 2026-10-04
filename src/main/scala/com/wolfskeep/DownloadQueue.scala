package com.wolfskeep

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.model.DateTime

import scala.collection.immutable.Queue
import scala.concurrent.duration._

object DownloadQueue {
  // Public command - mirrors Downloader.Fetch but allows internal messages in same protocol
  sealed trait Command
  final case class Fetch(url: String, replyTo: ActorRef[Downloader.Response], since: Option[DateTime] = None) extends Command

  // Internal commands
  private final case class DownloaderResponse(response: Downloader.Response) extends Command
  private case object Wakeup extends Command

  private case class State(
    waitingQueue: Queue[Fetch],
    outstanding: Map[String, Fetch],
    duplicates: Map[String, List[Fetch]],
    pausedUntil: Option[Deadline]
  )

  def apply(
    concurrencyLimit: Int,
    downloader: ActorRef[Downloader.Command],
    pauseDuration: FiniteDuration = 60.seconds
  ): Behavior[Command] =
    Behaviors.withTimers[Command] { timers =>
      Behaviors.setup { context =>
        val responseAdapter: ActorRef[Downloader.Response] =
          context.messageAdapter[Downloader.Response](DownloaderResponse)

        def processNextFetch(state: State): State = {
          state.pausedUntil match {
            case Some(deadline) if deadline.hasTimeLeft =>
              timers.startSingleTimer(Wakeup, deadline.timeLeft)
              state
            case _ =>
              if (state.outstanding.size < concurrencyLimit && state.waitingQueue.nonEmpty) {
                val (request, rest) = state.waitingQueue.dequeue
                val newState = state.outstanding.get(request.url) match {
                  case Some(_) =>
                    // a download for this URL is already in flight; share its response
                    state.copy(
                      waitingQueue = rest,
                      duplicates = state.duplicates + (request.url -> (request :: state.duplicates.getOrElse(request.url, Nil)))
                    )
                  case None =>
                    downloader ! Downloader.Fetch(request.url, responseAdapter, request.since)
                    state.copy(waitingQueue = rest, outstanding = state.outstanding + (request.url -> request))
                }
                processNextFetch(newState)
              } else {
                state
              }
          }
        }

        def onDownloaderResponse(state: State, response: Downloader.Response): State = {
          val url = response.url
          state.outstanding.get(url) match {
            case Some(issued) =>
              val duplicatesForUrl = state.duplicates.getOrElse(url, Nil)
              response match {
                case Downloader.TooManyRequests(_) =>
                  // politeness pause: requeue the issued request (duplicates stay attached
                  // to the URL and are answered when the retry completes) without replying
                  val newState = state.copy(
                    waitingQueue = state.waitingQueue.enqueue(issued),
                    outstanding = state.outstanding - url,
                    pausedUntil = Some(Deadline.now + pauseDuration)
                  )
                  processNextFetch(newState)
                case _ =>
                  issued.replyTo ! response
                  duplicatesForUrl.foreach(_.replyTo ! response)
                  val newState = state.copy(
                    outstanding = state.outstanding - url,
                    duplicates = state.duplicates - url
                  )
                  processNextFetch(newState)
              }
            case None =>
              // stray or duplicate reply (e.g. a late response after the request deadline
              // already failed this URL); late replies are expected with the tell protocol
              context.log.warn(s"Ignoring stray downloader response for $url")
              state
          }
        }

        def running(state: State): Behavior[Command] = Behaviors.receive {
          case (_, fetch: Fetch) =>
            val newState =
              if (state.outstanding.contains(fetch.url)) {
                state.copy(duplicates = state.duplicates + (fetch.url -> (fetch :: state.duplicates.getOrElse(fetch.url, Nil))))
              } else {
                state.copy(waitingQueue = state.waitingQueue.enqueue(fetch))
              }
            running(processNextFetch(newState))

          case (_, DownloaderResponse(response)) =>
            running(onDownloaderResponse(state, response))

          case (_, Wakeup) =>
            running(processNextFetch(state.copy(pausedUntil = None)))
        }

        running(State(Queue.empty, Map.empty, Map.empty, None))
      }
    }
}
