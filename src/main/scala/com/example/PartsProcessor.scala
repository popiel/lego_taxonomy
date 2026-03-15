package com.example

import akka.actor.typed.{ActorRef, Behavior, ActorSystem}
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.AskPattern._
import akka.util.Timeout
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext

object PartsProcessor {
  sealed trait Command
  case class ProcessParts(coloredParts: List[ColoredPart], replyTo: ActorRef[Response]) extends Command

  sealed trait Response
  case class ProcessedParts(parts: List[MatchedPart]) extends Response

  def apply(taxonomyDataHolder: ActorRef[TaxonomyDataHolder.Command]): Behavior[Command] = {
    Behaviors.setup { context =>
      implicit val ec: ExecutionContext = context.executionContext
      implicit val scheduler: akka.actor.typed.Scheduler = context.system.scheduler
      Behaviors.receiveMessage { message =>
        message match {
          case ProcessParts(coloredParts, replyTo) =>
            implicit val timeout: Timeout = Timeout(1.seconds)
            val taxonomyDataFuture = taxonomyDataHolder.ask(ref => TaxonomyDataHolder.GetTaxonomy(ref))
            
            taxonomyDataFuture.onComplete {
              case scala.util.Success(TaxonomyDataHolder.TaxonomyData(categories, parts)) =>
                val partMap = parts.flatMap(part => (part.partNumber :: part.altNumbers.toList).map(_ -> part)).toMap
                val matchedParts = coloredParts.map { cp =>
                  val legoPart = partMap.get(cp.partNumber)
                  MatchedPart(cp, legoPart)
                }
                val sortedParts = matchedParts.sorted
                replyTo ! ProcessedParts(sortedParts)
              case scala.util.Failure(ex) =>
                context.log.error(s"Failed to get taxonomy data: ${ex.getMessage}")
                replyTo ! ProcessedParts(Nil)
            }
            
            Behaviors.same
        }
      }
    }
  }
}
