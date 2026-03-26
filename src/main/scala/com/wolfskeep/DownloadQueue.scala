package com.wolfskeep

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{Behaviors, TimerScheduler}
import akka.actor.typed.scaladsl.AskPattern._
import akka.util.Timeout
import akka.http.scaladsl.model.DateTime
import java.util.concurrent.TimeoutException

import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

object DownloadQueue {
  // Public command - mirrors Downloader.Fetch but allows internal messages in same protocol
  sealed trait Command
  final case class Fetch(url: String, replyTo: ActorRef[Downloader.Response], since: Option[DateTime] = None) extends Command
  
  // Internal commands
  private final case class WrappedResult(result: Try[Downloader.Response], request: Fetch) extends Command
  private case object Wakeup extends Command
  
  private case class State(
    waitingQueue: collection.immutable.Queue[Fetch],
    activeRequests: Set[Fetch],
    pausedUntil: Option[Deadline]
  )
  
  def apply(
    concurrencyLimit: Int,
    downloader: ActorRef[Downloader.Command],
    askTimeout: FiniteDuration = 5.seconds,
    pauseDuration: FiniteDuration = 60.seconds
  ): Behavior[Command] =
    Behaviors.withTimers[Command] { timers =>
      Behaviors.setup { context =>
        implicit val scheduler = context.system.scheduler
        implicit val timeout: Timeout = askTimeout
        implicit val ec = context.executionContext
        
        def processNextFetch(state: State): State = {
          state.pausedUntil match {
            case Some(deadline) if deadline.hasTimeLeft =>
              timers.startSingleTimer(Wakeup, deadline.timeLeft)
              state
            case _ =>
              if (state.activeRequests.size < concurrencyLimit && state.waitingQueue.nonEmpty) {
                val (request, newQueue) = state.waitingQueue.dequeue
                context.ask(downloader, (ref: ActorRef[Downloader.Response]) =>
                  Downloader.Fetch(request.url, ref, request.since)
                ) {
                  case Success(response) => WrappedResult(Success(response), request)
                  case Failure(ex) => WrappedResult(Failure(ex), request)
                }
                state.copy(
                  waitingQueue = newQueue,
                  activeRequests = state.activeRequests + request
                )
              } else {
                state
              }
          }
        }
        
        def running(state: State): Behavior[Command] = Behaviors.receive {
          case (_, fetch: Fetch) =>
            val newState = state.copy(waitingQueue = state.waitingQueue.enqueue(fetch))
            running(processNextFetch(newState))
          
          case (_, WrappedResult(Success(Downloader.Downloaded(url, content)), request)) =>
            request.replyTo ! Downloader.Downloaded(url, content)
            val newActive = state.activeRequests - request
            running(processNextFetch(state.copy(activeRequests = newActive)))
          
          case (_, WrappedResult(Success(Downloader.NotChanged(url)), request)) =>
            request.replyTo ! Downloader.NotChanged(url)
            val newActive = state.activeRequests - request
            running(processNextFetch(state.copy(activeRequests = newActive)))
          
          case (_, WrappedResult(Success(Downloader.Failed(url, reason)), request)) =>
            request.replyTo ! Downloader.Failed(url, reason)
            val newActive = state.activeRequests - request
            running(processNextFetch(state.copy(activeRequests = newActive)))
          
          case (_, WrappedResult(Success(Downloader.TooManyRequests(url)), request)) =>
            val newWaiting = state.waitingQueue.enqueue(request)
            val newPaused = Some(Deadline.now + pauseDuration)
            val newActive = state.activeRequests - request
            val newState = state.copy(waitingQueue = newWaiting, pausedUntil = newPaused, activeRequests = newActive)
            running(processNextFetch(newState))
          
          case (_, WrappedResult(Failure(ex), request)) =>
            ex match {
              case _: TimeoutException =>
                val newWaiting = state.waitingQueue.enqueue(request)
                val newActive = state.activeRequests - request
                running(processNextFetch(state.copy(waitingQueue = newWaiting, activeRequests = newActive)))
              case _ =>
                request.replyTo ! Downloader.Failed(request.url, ex.getMessage)
                val newActive = state.activeRequests - request
                running(processNextFetch(state.copy(activeRequests = newActive)))
            }
          
          case (_, Wakeup) =>
            running(processNextFetch(state.copy(pausedUntil = None)))
        }
        
        running(State(collection.immutable.Queue.empty, Set.empty, None))
      }
    }
}