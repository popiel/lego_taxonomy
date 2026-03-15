package com.example

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors

object TaxonomyDataHolder {
  sealed trait Command
  case class SetTaxonomy(categories: Set[Category], parts: List[LegoPart]) extends Command
  case class GetTaxonomy(replyTo: ActorRef[Response]) extends Command

  sealed trait Response
  case class TaxonomyData(categories: Set[Category], parts: List[LegoPart]) extends Response

  def apply(): Behavior[Command] = Behaviors.setup { context =>
    idle(Set.empty, Nil)
  }

  private def idle(categories: Set[Category], parts: List[LegoPart]): Behavior[Command] = Behaviors.receive { (context, message) =>
    message match {
      case SetTaxonomy(newCategories, newParts) =>
        val catCsv = TaxonomySortMain.buildCategoriesCsv(newCategories)
        val partCsv = TaxonomySortMain.buildPartsCsv(newParts)
        TaxonomySortMain.writeToFile("categories.csv", catCsv)
        TaxonomySortMain.writeToFile("parts.csv", partCsv)
        context.log.info(s"Taxonomy saved: ${newCategories.size} categories, ${newParts.size} parts")
        idle(newCategories, newParts)

      case GetTaxonomy(replyTo) =>
        replyTo ! TaxonomyData(categories, parts)
        Behaviors.same
    }
  }
}
