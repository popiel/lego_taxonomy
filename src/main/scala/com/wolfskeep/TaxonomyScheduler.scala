package com.wolfskeep

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import scala.concurrent.duration._
import java.time.{LocalTime, LocalDateTime, ZonedDateTime, ZoneId}

object TaxonomyScheduler {
  sealed trait Command
  case object FetchTaxonomy extends Command
  private case object AugmentationCompleteResult extends Command
  private case class TaxonomyFetchedFailed(reason: Throwable) extends Command
  private case object CycleWatchdog extends Command

  private val FetchHour = 3
  private val FetchMinute = 0

  def apply(
    fetcherRef: ActorRef[TaxonomyFetcher.Command],
    taxonomyDataHolder: ActorRef[TaxonomyHolder.Command],
    cycleTimeout: FiniteDuration = 35.minutes
  ): Behavior[Command] = {
    Behaviors.withTimers[Command] { timers =>
      Behaviors.setup { context =>
        implicit val ec: scala.concurrent.ExecutionContext = context.executionContext

        // long-lived adapter: with the tell-based protocol the fetcher may reply
        // more than once per request (late replies), so no one-shot ask is used
        val responseAdapter: ActorRef[TaxonomyFetcher.Response] =
          context.messageAdapter[TaxonomyFetcher.Response] {
            case TaxonomyFetcher.AugmentationComplete => AugmentationCompleteResult
            case TaxonomyFetcher.Failed(reason)      => TaxonomyFetchedFailed(reason)
          }

        def scheduleNextFetch(): Unit = {
          val delay = calculateDelayUntil3am()
          context.system.scheduler.scheduleOnce(delay, new Runnable {
            override def run(): Unit = {
              context.self ! FetchTaxonomy
            }
          })
        }

        Behaviors.receiveMessage[Command] { message =>
          message match {
            case FetchTaxonomy =>
              fetcherRef ! TaxonomyFetcher.GetTaxonomy(responseAdapter)
              // if the fetcher is busy mid-cycle it ignores the request; the
              // watchdog re-issues it so the daily chain never breaks
              timers.startSingleTimer(CycleWatchdog, cycleTimeout)
              Behaviors.same

            case AugmentationCompleteResult =>
              timers.cancel(CycleWatchdog)
              // the fetcher publishes SetTaxonomy/AugmentPart to the holder itself
              context.log.info("Taxonomy fetch cycle complete")
              scheduleNextFetch()
              Behaviors.same

            case TaxonomyFetchedFailed(reason) =>
              timers.cancel(CycleWatchdog)
              context.log.error(s"Taxonomy fetch failed: ${reason.getMessage}")
              scheduleNextFetch()
              Behaviors.same

            case CycleWatchdog =>
              context.log.warn(s"Taxonomy fetch cycle did not complete within $cycleTimeout; re-issuing the fetch request")
              context.self ! FetchTaxonomy
              Behaviors.same
          }
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
