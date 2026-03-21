package com.wolfskeep.rebrickable

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.AskPattern._
import akka.util.Timeout

import java.time.{LocalTime, LocalDateTime, ZonedDateTime, ZoneId}

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import scala.util.{Success, Failure}

object RebrickableSchedulerActor {
  sealed trait Command
  case object FetchRebrickable extends Command
  private case class FetchedResult(data: Data) extends Command
  private case class FetchedFailed(reason: Throwable) extends Command

  private val FetchHour = 3
  private val FetchMinute = 0

  def apply(
    fetcherRef: ActorRef[RebrickableFetcherActor.Command],
    dataActor: ActorRef[RebrickableDataActor.Command]
  ): Behavior[Command] = {
    Behaviors.setup { context =>
      implicit val ec: ExecutionContext = context.executionContext
      implicit val scheduler: akka.actor.typed.Scheduler = context.system.scheduler
      implicit val timeout: Timeout = Timeout(30.minutes)

      def scheduleNextFetch(): Unit = {
        val delay = calculateDelayUntil3am()
        context.system.scheduler.scheduleOnce(delay, new Runnable {
          override def run(): Unit = {
            context.self ! FetchRebrickable
          }
        })
        context.log.info("RebrickableScheduler: Next fetch scheduled in {} hours", delay.toHours)
      }

      Behaviors.receiveMessage[Command] { message =>
        message match {
          case FetchRebrickable =>
            context.ask(fetcherRef, (replyTo: ActorRef[RebrickableFetcherActor.Command]) => RebrickableFetcherActor.FetchAll) {
              case Success(RebrickableFetcherActor.FetchResult(data)) =>
                FetchedResult(data)
              case Success(RebrickableFetcherActor.FetchFailed(reason)) =>
                FetchedFailed(reason)
              case Failure(ex) =>
                FetchedFailed(ex)
            }
            Behaviors.same

          case FetchedResult(data) =>
            context.log.info(
              "RebrickableScheduler: Fetched {} parts, {} colors, {} elements, {} sets, {} inventories, {} inventoryParts",
              data.parts.size: Integer, data.colors.size: Integer, data.elements.size: Integer,
              data.sets.size: Integer, data.inventories.size: Integer, data.inventoryParts.size: Integer
            )
            scheduleNextFetch()
            Behaviors.same

          case FetchedFailed(reason) =>
            context.log.error("RebrickableScheduler: Fetch failed: {}", reason.getMessage)
            scheduleNextFetch()
            Behaviors.same
        }
      }
    }
  }

  private def calculateDelayUntil3am(): FiniteDuration = {
    val now = ZonedDateTime.now(ZoneId.systemDefault())
    val targetTime = LocalDateTime.of(now.toLocalDate, LocalTime.of(FetchHour, FetchMinute))
    val targetWithZone = targetTime.atZone(ZoneId.systemDefault())

    val target = if (targetWithZone.isAfter(now)) {
      targetWithZone
    } else {
      targetWithZone.plusDays(1)
    }

    val duration = java.time.Duration.between(now, target)
    duration.toMillis.milliseconds
  }
}
