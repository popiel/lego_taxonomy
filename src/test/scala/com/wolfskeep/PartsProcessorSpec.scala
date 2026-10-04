package com.wolfskeep

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.BeforeAndAfterAll
import com.wolfskeep.rebrickable.RebrickableHolder

import scala.concurrent.duration._

class PartsProcessorSpec extends ScalaTestWithActorTestKit with AnyWordSpecLike with BeforeAndAfterAll {

  private val taxonomyParts = scala.io.Source.fromFile("parts.csv").getLines().drop(1).map { line =>
    val fields = line.split(",").map(_.trim.replaceAll("^\"|\"$", ""))
    val partNumber = fields(0)
    val name = fields(1)
    val categories = (2 to Math.min(5, fields.length - 1)).map { idx =>
      Category((idx - 1).toString, fields(idx), None)
    }.filter(_.name.nonEmpty).toList
    LegoPart(
      partNumber = partNumber,
      name = name,
      categories = categories,
      sequenceNumber = 0,
      altNumbers = Set.empty,
      imageUrl = None,
      imageWidth = None,
      imageHeight = None
    )
  }.toList

  private val rebrickableDataActor = spawn(RebrickableHolder())
  private val taxonomyDataHolder = spawn(TaxonomyHolder(rebrickableDataActor))

  taxonomyDataHolder ! TaxonomyHolder.SetTaxonomy(TaxonomyData(Set.empty, taxonomyParts))

  Thread.sleep(100)

  "PartsProcessor" should {
    "return same number of MatchedParts as input ColoredParts when all parts match taxonomy directly" in {
      val matchingColoredParts = taxonomyParts.take(10).map { legoPart =>
        ColoredPart(
          partNumber = legoPart.partNumber,
          color = "Red",
          quantity = 1,
          name = legoPart.name,
          elementId = None
        )
      }

      val partsProcessor = spawn(PartsProcessor(taxonomyDataHolder))
      val probe = createTestProbe[PartsProcessor.Response]()

      partsProcessor ! PartsProcessor.ProcessParts(matchingColoredParts, probe.ref)

      val response = probe.expectMessageType[PartsProcessor.ProcessedParts](10.seconds)

      response.parts.size should ===(matchingColoredParts.size)
      response.parts.forall(_.legoPart.isDefined) should be(true)
    }

    "preserve ColoredPart name, color, quantity, and partNumber through processSinglePart" in {
      val coloredPartWithDetails = ColoredPart(
        partNumber = "3001",
        name = "2x4 Brick",
        color = "Red",
        quantity = 5,
        elementId = Some("6331694")
      )

      val partsProcessor = spawn(PartsProcessor(taxonomyDataHolder))
      val probe = createTestProbe[PartsProcessor.Response]()

      partsProcessor ! PartsProcessor.ProcessParts(List(coloredPartWithDetails), probe.ref)

      val response = probe.expectMessageType[PartsProcessor.ProcessedParts](10.seconds)

      response.parts.size should ===(1)
      val result = response.parts.head
      result.coloredPart.partNumber should ===("3001")
      result.coloredPart.name should ===("2x4 Brick")
      result.coloredPart.color should ===("Red")
      result.coloredPart.quantity should ===(5)
    }

    "resolve a patterned part number to its base taxonomy part as Modified" in {
      val coloredPart = ColoredPart(
        partNumber = "3001xyz",
        name = "Brick 2 x 4 with Special Print",
        color = "Red",
        quantity = 2,
        elementId = None
      )

      val partsProcessor = spawn(PartsProcessor(taxonomyDataHolder))
      val probe = createTestProbe[PartsProcessor.Response]()

      partsProcessor ! PartsProcessor.ProcessParts(List(coloredPart), probe.ref)

      val response = probe.expectMessageType[PartsProcessor.ProcessedParts](10.seconds)

      response.parts.size should ===(1)
      val result = response.parts.head
      result.legoPart.isDefined should be(true)
      result.legoPart.get.partNumber should ===("3001xyz")
      result.legoPart.get.name should include("(modified)")
      result.legoPart.get.categories should not be empty
    }

    "infer categories from a sibling part name prefix in the same upload" in {
      val sibling = ColoredPart(
        partNumber = "3001",
        name = "XYZ",
        color = "Red",
        quantity = 1,
        elementId = None
      )
      val miss = ColoredPart(
        partNumber = "99999",
        name = "XYZ Special Edition",
        color = "Blue",
        quantity = 3,
        elementId = None
      )

      val partsProcessor = spawn(PartsProcessor(taxonomyDataHolder))
      val probe = createTestProbe[PartsProcessor.Response]()

      partsProcessor ! PartsProcessor.ProcessParts(List(sibling, miss), probe.ref)

      val response = probe.expectMessageType[PartsProcessor.ProcessedParts](10.seconds)

      response.parts.size should ===(2)
      val siblingResult = response.parts.find(_.coloredPart.partNumber == "3001").get
      val missResult = response.parts.find(_.coloredPart.partNumber == "99999").get
      siblingResult.legoPart.isDefined should be(true)
      missResult.legoPart.isDefined should be(true)
      missResult.categoriesGuessed should be(true)
      missResult.legoPart.get.name should include("(guessed)")
      missResult.legoPart.get.categories should ===(siblingResult.legoPart.get.categories)
    }
  }
}
