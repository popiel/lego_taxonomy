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

  def apply(
    taxonomyDataHolder: ActorRef[TaxonomyDataHolder.Command],
    bricksetPartCache: ActorRef[BricksetPartCache.Command]
  ): Behavior[Command] = {
    Behaviors.setup { context =>
      implicit val ec: ExecutionContext = context.executionContext
      implicit val scheduler: akka.actor.typed.Scheduler = context.system.scheduler
      Behaviors.receiveMessage { message =>
        message match {
          case ProcessParts(coloredParts, replyTo) =>
            val taxonomyTimeout: Timeout = Timeout(1.seconds)
            val taxonomyDataFuture = taxonomyDataHolder.ask(ref => TaxonomyDataHolder.GetTaxonomy(ref))(taxonomyTimeout, scheduler)

            taxonomyDataFuture.onComplete {
              case scala.util.Success(TaxonomyDataHolder.TaxonomyData(categories, parts)) =>
                val partMap = parts.flatMap(part => (part.partNumber :: part.altNumbers.toList).map(_ -> part)).toMap
                val matchedPartsWithoutBrickset = coloredParts.map { cp =>
                  val legoPart = partMap.get(cp.partNumber)
                  MatchedPart(cp, legoPart)
                }

                val unmatchedParts = matchedPartsWithoutBrickset.filter(_.legoPart.isEmpty)
                val matchedPartsWithBrickset = matchedPartsWithoutBrickset.filter(_.legoPart.isDefined)

                if (unmatchedParts.isEmpty) {
                  val sortedParts = matchedPartsWithoutBrickset.sorted
                  replyTo ! ProcessedParts(sortedParts)
                } else {
                  val bricksetTimeout: Timeout = Timeout(30.seconds)
                  val bricksetFutures = unmatchedParts.map { mp =>
                    bricksetPartCache.ask(BricksetPartCache.GetPart(mp.coloredPart.partNumber, _))(bricksetTimeout, scheduler).map { bricksetPart =>
                      (mp.coloredPart.partNumber, bricksetPart)
                    }
                  }

                  import scala.concurrent.Future
                  val allFutures: Future[Seq[(String, Option[LegoPart])]] = Future.sequence(bricksetFutures)

                  allFutures.onComplete {
                    case scala.util.Success(bricksetResults) =>
                      val bricksetMap = bricksetResults.toMap
                      val finalMatchedParts = matchedPartsWithBrickset ++ unmatchedParts.map { mp =>
                        val bricksetPart = bricksetMap.get(mp.coloredPart.partNumber).flatten
                        MatchedPart(mp.coloredPart, bricksetPart)
                      }
                      val sortedParts = finalMatchedParts.sorted
                      replyTo ! ProcessedParts(sortedParts)
                    case scala.util.Failure(ex) =>
                      context.log.error(s"Failed to get Brickset data: ${ex.getMessage}")
                      val sortedParts = matchedPartsWithoutBrickset.sorted
                      replyTo ! ProcessedParts(sortedParts)
                  }
                }

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
