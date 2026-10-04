package com.wolfskeep

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.AskPattern._
import akka.actor.typed.scaladsl.Behaviors
import akka.util.Timeout

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

object PartsProcessor {
  sealed trait Command
  case class ProcessParts(coloredParts: List[ColoredPart], replyTo: ActorRef[Response]) extends Command

  sealed trait Response
  case class ProcessedParts(parts: List[MatchedPart]) extends Response

  def apply(
    taxonomyDataHolder: ActorRef[TaxonomyHolder.Command],
    lookupAsk: FiniteDuration = Timeouts.service.lookupAsk
  ): Behavior[Command] = {
    Behaviors.setup { context =>
      implicit val ec: ExecutionContext = context.executionContext
      implicit val scheduler: akka.actor.typed.Scheduler = context.system.scheduler
      val logger = context.log

      Behaviors.receiveMessage {
        case ProcessParts(coloredParts, replyTo) =>
          logger.info(s"ProcessParts with ${coloredParts.size} input parts")
          implicit val lookupTimeout: Timeout = Timeout(lookupAsk)

          val requests = coloredParts.map(cp =>
            TaxonomyHolder.LookupPartRequest(cp.partNumber, cp.elementId, cp.name))
          taxonomyDataHolder.ask(ref => TaxonomyHolder.LookupParts(requests, ref)).onComplete {
            case scala.util.Success(results) =>
              val matched = coloredParts.zip(results).map { case (cp, result) =>
                buildMatchedPart(cp, result)
              }
              val (withCategories, withoutCategories) =
                matched.partition(_.legoPart.exists(_.categories.nonEmpty))
              val prefixMatched = withoutCategories.map(mp => uploadLocalPrefixMatch(mp, withCategories))
              val sortedParts = (withCategories ++ prefixMatched).sorted
              logger.info(s"ProcessParts replying with ${sortedParts.size} output parts")
              replyTo ! ProcessedParts(sortedParts)

            case scala.util.Failure(ex) =>
              logger.error(s"Failed to get taxonomy data: ${ex.getMessage}")
              replyTo ! ProcessedParts(Nil)
          }

          Behaviors.same
      }
    }
  }

  private def buildMatchedPart(coloredPart: ColoredPart, result: TaxonomyHolder.LookupResult): MatchedPart =
    MatchedPart(coloredPart, result.legoPart, result.categoriesGuessed)

  private def uploadLocalPrefixMatch(mp: MatchedPart, candidates: List[MatchedPart]): MatchedPart = {
    val lowerName = mp.coloredPart.name.toLowerCase

    val prefixMatch = candidates
      .flatMap(_.legoPart)
      .flatMap { legoPart =>
        candidates.find(c => c.legoPart.contains(legoPart)).map { c => (legoPart, c.coloredPart.name) }
      }
      .filter { case (_, coloredPartName) =>
        lowerName.startsWith(coloredPartName.toLowerCase)
      }
      .sortBy { case (_, coloredPartName) => -coloredPartName.length }
      .headOption

    prefixMatch match {
      case Some((matchedLegoPart, _)) =>
        val newName = s"${matchedLegoPart.name} (guessed)"
        val updatedLegoPart = mp.legoPart.getOrElse(createGuessedLegoPart(newName, matchedLegoPart.categories))
          .copy(name = newName, categories = matchedLegoPart.categories)
        MatchedPart(mp.coloredPart, Some(updatedLegoPart), categoriesGuessed = true)
      case None =>
        mp
    }
  }

  private def createGuessedLegoPart(name: String, categories: List[Category]): LegoPart =
    LegoPart(
      partNumber = "",
      name = name,
      categories = categories,
      sequenceNumber = 0,
      altNumbers = Set.empty,
      imageUrl = None,
      imageWidth = None,
      imageHeight = None
    )
}
