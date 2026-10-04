package com.wolfskeep

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.AskPattern._
import akka.util.Timeout
import scala.concurrent.duration._
import scala.util.Try

import com.wolfskeep.rebrickable.{Data, RebrickableHolder}

object TaxonomyHolder {
  sealed trait Command
  case class SetTaxonomy(taxonomyData: TaxonomyData) extends Command
  case class AugmentPart(partNumber: String, altNumbers: Set[String]) extends Command
  case class GetTaxonomy(replyTo: ActorRef[Response]) extends Command
  case class LookupParts(requests: List[LookupPartRequest], replyTo: ActorRef[List[LookupResult]]) extends Command

  // internal
  private case class LookupWithRebrickable(
    requests: List[LookupPartRequest],
    replyTo: ActorRef[List[LookupResult]],
    rebrickableData: Data
  ) extends Command

  sealed trait Response
  case class TaxonomyDataResponse(taxonomyData: TaxonomyData) extends Response

  case class LookupPartRequest(partNumber: String, elementId: Option[String], name: String)

  sealed trait LookupVia
  case object Exact extends LookupVia
  case object AltNumber extends LookupVia
  case object Modified extends LookupVia
  case object Guessed extends LookupVia
  case object Miss extends LookupVia

  case class LookupResult(
    request: LookupPartRequest,
    legoPart: Option[LegoPart],
    via: LookupVia,
    categoriesGuessed: Boolean = false
  )

  def apply(rebrickableRef: ActorRef[RebrickableHolder.Command]): Behavior[Command] = Behaviors.setup { context =>
    idle(rebrickableRef, None, Map.empty)
  }

  private def merged(taxonomyData: TaxonomyData, overlay: Map[String, Set[String]]): TaxonomyData =
    if (overlay.isEmpty) taxonomyData
    else taxonomyData.copy(parts = taxonomyData.parts.map { part =>
      overlay.get(part.partNumber).map(altNumbers => part.copy(altNumbers = altNumbers)).getOrElse(part)
    })

  private def idle(
    rebrickableRef: ActorRef[RebrickableHolder.Command],
    taxonomyData: Option[TaxonomyData],
    overlay: Map[String, Set[String]]
  ): Behavior[Command] = Behaviors.receive { (context, message) =>
    message match {
      case SetTaxonomy(newTaxonomyData) =>
        val catCsv = TaxonomySortMain.buildCategoriesCsv(newTaxonomyData.categories)
        val partCsv = TaxonomySortMain.buildPartsCsv(newTaxonomyData.parts)
        TaxonomySortMain.writeToFile("categories.csv", catCsv)
        TaxonomySortMain.writeToFile("parts.csv", partCsv)
        context.log.info(s"Taxonomy saved: ${newTaxonomyData.categories.size} categories, ${newTaxonomyData.parts.size} parts")
        idle(rebrickableRef, Some(newTaxonomyData), Map.empty)

      case AugmentPart(partNumber, altNumbers) =>
        taxonomyData match {
          case Some(data) if data.parts.exists(_.partNumber == partNumber) =>
            idle(rebrickableRef, taxonomyData, overlay + (partNumber -> altNumbers))
          case _ =>
            context.log.warn(s"Ignoring AugmentPart for unknown part $partNumber")
            Behaviors.same
        }

      case GetTaxonomy(replyTo) =>
        taxonomyData match {
          case Some(data) =>
            val mergedData = merged(data, overlay)
            replyTo ! TaxonomyDataResponse(mergedData)
            idle(rebrickableRef, Some(mergedData), Map.empty)
          case None =>
            replyTo ! TaxonomyDataResponse(TaxonomyData(Set.empty, Nil))
            Behaviors.same
        }

      case LookupParts(requests, replyTo) =>
        implicit val timeout: Timeout = Timeout(1.second)
        context.ask(rebrickableRef, RebrickableHolder.GetData) {
          case scala.util.Success(data) =>
            LookupWithRebrickable(requests, replyTo, data)
          case scala.util.Failure(ex) =>
            context.log.warn(s"Failed to get rebrickable data for lookup, resolving without design IDs: ${ex.getMessage}")
            LookupWithRebrickable(requests, replyTo, Data(Nil, Nil, Nil, Nil, Nil, Nil))
        }
        Behaviors.same

      case LookupWithRebrickable(requests, replyTo, rebrickableData) =>
        val mergedData = taxonomyData.map(merged(_, overlay)).getOrElse(TaxonomyData(Set.empty, Nil))
        val results = requests.map(resolve(mergedData, rebrickableData))
        replyTo ! results
        idle(rebrickableRef, taxonomyData.map(merged(_, overlay)), Map.empty)
    }
  }

  private def resolve(taxonomyData: TaxonomyData, rebrickableData: Data)(request: LookupPartRequest): LookupResult = {
    def fuzzyResult(foundPart: Option[LegoPart], via: LookupVia): LookupResult = {
      val searchResults = taxonomyData.searchByName(request.name)
      val queryWords = TaxonomyData.tokenize(request.name).toSet
      val subsetMatch = searchResults.find { case (part, _) =>
        TaxonomyData.tokenize(part.name).toSet.subsetOf(queryWords)
      }

      subsetMatch match {
        case Some((matchedPart, _)) =>
          val newName = s"${matchedPart.name} (guessed)"
          LookupResult(
            request,
            Some(foundPart.getOrElse(guessLegoPart(matchedPart.categories)).copy(name = newName, categories = matchedPart.categories)),
            Guessed,
            categoriesGuessed = true
          )
        case None =>
          val top5 = searchResults.take(5)
          val commonPrefix = taxonomyData.findCommonCategoryPrefix(top5.map(_._1))
          if (commonPrefix.nonEmpty) {
            val bestMatch = top5.find { case (part, _) =>
              part.categories.zip(commonPrefix).forall { case (cat, prefixCat) => cat == prefixCat }
            }
            val newName = bestMatch.map { case (part, _) => s"${part.name} (guessed)" }.getOrElse(request.name)
            LookupResult(
              request,
              Some(foundPart.getOrElse(guessLegoPart(commonPrefix)).copy(name = newName, categories = commonPrefix)),
              Guessed,
              categoriesGuessed = true
            )
          } else {
            foundPart.map(part => LookupResult(request, Some(part), via)).getOrElse(LookupResult(request, None, Miss))
          }
      }
    }

    taxonomyData.findPart(request.partNumber) match {
      case Some(part) =>
        val via = if (part.partNumber == request.partNumber) Exact else AltNumber
        if (part.categories.nonEmpty) LookupResult(request, Some(part), via)
        else fuzzyResult(Some(part), via)
      case None =>
        val designIdOpt: Option[String] = request.elementId.flatMap { elementId =>
          Try(elementId.toLong).toOption.flatMap(rebrickableData.elementIdToDesignId)
        }
        taxonomyData.findBasePart(request.partNumber).orElse(designIdOpt.flatMap(taxonomyData.findBasePart)) match {
          case Some(part) if part.categories.nonEmpty =>
            LookupResult(request, Some(part), Modified)
          case Some(part) =>
            fuzzyResult(Some(part), Modified)
          case None =>
            fuzzyResult(None, Miss)
        }
    }
  }

  private def guessLegoPart(categories: List[Category]): LegoPart =
    LegoPart(
      partNumber = "",
      name = "",
      categories = categories,
      sequenceNumber = 0,
      altNumbers = Set.empty,
      imageUrl = None,
      imageWidth = None,
      imageHeight = None
    )
}
