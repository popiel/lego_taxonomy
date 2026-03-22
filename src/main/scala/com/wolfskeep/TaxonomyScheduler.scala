package com.wolfskeep

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.AskPattern._
import akka.util.Timeout
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import scala.util.{Success, Failure}
import java.time.{LocalTime, LocalDateTime, ZonedDateTime, ZoneId}

object TaxonomyScheduler {
  sealed trait Command
  case object FetchTaxonomy extends Command
  private case class TaxonomyFetchedResult(taxonomyData: TaxonomyData) extends Command
  private case class TaxonomyFetchedFailed(reason: Throwable) extends Command

  private val FetchHour = 3
  private val FetchMinute = 0

  def apply(fetcherRef: ActorRef[TaxonomyFetcher.Command], taxonomyDataHolder: ActorRef[TaxonomyHolder.Command]): Behavior[Command] = {
    Behaviors.setup { context =>
      implicit val ec: ExecutionContext = context.executionContext
      implicit val scheduler: akka.actor.typed.Scheduler = context.system.scheduler
      implicit val timeout: Timeout = Timeout(30.minutes)

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
            context.ask(fetcherRef, TaxonomyFetcher.GetTaxonomy) {
              case Success(TaxonomyFetcher.TaxonomyFetched(taxonomyData)) =>
                TaxonomyFetchedResult(taxonomyData)
              case Success(TaxonomyFetcher.Failed(reason)) =>
                TaxonomyFetchedFailed(reason)
              case Failure(ex) =>
                TaxonomyFetchedFailed(ex)
            }
            Behaviors.same

          case TaxonomyFetchedResult(taxonomyData) =>
            taxonomyDataHolder ! TaxonomyHolder.SetTaxonomy(taxonomyData)
            context.log.info(s"Taxonomy fetched: ${taxonomyData.categories.size} categories, ${taxonomyData.parts.size} parts")
            scheduleNextFetch()
            Behaviors.same

          case TaxonomyFetchedFailed(reason) =>
            context.log.error(s"Taxonomy fetch failed: ${reason.getMessage}")
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
