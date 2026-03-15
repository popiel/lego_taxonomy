package com.example

import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.AskPattern._
import akka.util.Timeout
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import java.time.{LocalTime, LocalDateTime, ZonedDateTime, ZoneId}

object TaxonomyScheduler {
  sealed trait Command
  case object FetchTaxonomy extends Command

  private val FetchHour = 3
  private val FetchMinute = 0

  def apply(taxonomyDataHolder: ActorRef[TaxonomyDataHolder.Command]): Behavior[Command] = {
    Behaviors.setup { context =>
      implicit val ec: ExecutionContext = context.executionContext
      implicit val scheduler: akka.actor.typed.Scheduler = context.system.scheduler
      implicit val timeout: Timeout = Timeout(1.seconds)

      def scheduleNextFetch(): Unit = {
        val delay = calculateDelayUntil3am()
        context.system.scheduler.scheduleOnce(delay, new Runnable {
          override def run(): Unit = {
            context.self ! FetchTaxonomy
          }
        })
      }

      Behaviors.receiveMessage { message =>
        message match {
          case FetchTaxonomy =>
            val fetcherSystem = context.system.asInstanceOf[ActorSystem[TaxonomyFetcher.Command]]
            val fetcherAsk = fetcherSystem.ask(ref => TaxonomyFetcher.GetTaxonomy(ref))
            fetcherAsk.onComplete {
              case scala.util.Success(TaxonomyFetcher.TaxonomyFetched(categories, parts)) =>
                taxonomyDataHolder ! TaxonomyDataHolder.SetTaxonomy(categories, parts,
                  context.system.systemActorOf(Behaviors.receiveMessage[TaxonomyDataHolder.Response](_ => Behaviors.stopped), "reply-handler"))
                context.log.info(s"Taxonomy fetched: ${categories.size} categories, ${parts.size} parts")
              case scala.util.Success(TaxonomyFetcher.Failed(reason)) =>
                context.log.error(s"Taxonomy fetch failed: ${reason.getMessage}")
              case scala.util.Failure(ex) =>
                context.log.error(s"Taxonomy fetch failed: ${ex.getMessage}", ex)
            }
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
