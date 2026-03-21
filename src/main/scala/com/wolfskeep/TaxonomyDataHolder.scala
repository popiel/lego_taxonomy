package com.wolfskeep

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors

object TaxonomyDataHolder {
  sealed trait Command
  case class SetTaxonomy(taxonomyData: TaxonomyData) extends Command
  case class GetTaxonomy(replyTo: ActorRef[Response]) extends Command
  case class SearchByName(query: String, replyTo: ActorRef[Response]) extends Command

  sealed trait Response
  case class TaxonomyDataResponse(taxonomyData: TaxonomyData) extends Response
  case class SearchResult(parts: List[(LegoPart, Int)]) extends Response

  def apply(): Behavior[Command] = Behaviors.setup { context =>
    idle(None)
  }

  private def idle(taxonomyData: Option[TaxonomyData]): Behavior[Command] = Behaviors.receive { (context, message) =>
    message match {
      case SetTaxonomy(newTaxonomyData) =>
        val catCsv = TaxonomySortMain.buildCategoriesCsv(newTaxonomyData.categories)
        val partCsv = TaxonomySortMain.buildPartsCsv(newTaxonomyData.parts)
        TaxonomySortMain.writeToFile("categories.csv", catCsv)
        TaxonomySortMain.writeToFile("parts.csv", partCsv)
        context.log.info(s"Taxonomy saved: ${newTaxonomyData.categories.size} categories, ${newTaxonomyData.parts.size} parts, ${newTaxonomyData.wordIndex.size} index entries")
        idle(Some(newTaxonomyData))

      case GetTaxonomy(replyTo) =>
        taxonomyData match {
          case Some(data) => replyTo ! TaxonomyDataResponse(data)
          case None => replyTo ! TaxonomyDataResponse(TaxonomyData(Set.empty, Nil))
        }
        Behaviors.same

      case SearchByName(query, replyTo) =>
        val results = taxonomyData.map(_.searchByName(query)).getOrElse(Nil)
        replyTo ! SearchResult(results)
        Behaviors.same
    }
  }
}