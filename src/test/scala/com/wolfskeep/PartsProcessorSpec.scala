package com.wolfskeep

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.BeforeAndAfterAll
import com.wolfskeep.rebrickable.RebrickableDataActor

import scala.concurrent.Await
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

  private val cache = spawn(DiskCache())
  private val downloader = spawn(CachedDownloader(cache))
  private val taxonomyDataHolder = spawn(TaxonomyDataHolder())
  private val bricklinkActor = spawn(BricklinkActor(cache))
  private val rebrickableDataActor = spawn(RebrickableDataActor())

  taxonomyDataHolder ! TaxonomyDataHolder.SetTaxonomy(TaxonomyData(Set.empty, taxonomyParts))
  
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
      
      val partsProcessor = spawn(PartsProcessor(taxonomyDataHolder, downloader, bricklinkActor, rebrickableDataActor))
      val probe = createTestProbe[PartsProcessor.Response]()
      
      partsProcessor ! PartsProcessor.ProcessParts(matchingColoredParts, probe.ref)
      
      val response = probe.expectMessageType[PartsProcessor.ProcessedParts](10.seconds)
      
      response.parts.size should === (matchingColoredParts.size)
      response.parts.forall(_.legoPart.isDefined) should be (true)
    }
  }
}
