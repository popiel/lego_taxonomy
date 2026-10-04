package com.wolfskeep

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import akka.actor.typed.ActorRef
import akka.actor.typed.scaladsl.Behaviors
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.matchers.should.Matchers

import com.wolfskeep.rebrickable.{Data, Element, RebrickableHolder}

import scala.concurrent.duration._

class TaxonomyHolderSpec extends ScalaTestWithActorTestKit with AnyWordSpecLike with Matchers {

  private val taxonomyParts = List(
    LegoPart("3001", "Brick 2 x 4", List(Category("1", "Basic", None)), 1, Set.empty),
    LegoPart("3069", "Tile 1 x 2", List(Category("2", "Tiles", None)), 2, Set.empty)
  )

  private def taxonomyData: TaxonomyData = TaxonomyData(Set.empty, taxonomyParts)

  private val basic = Category("1", "Basic", None)
  private val tiles = Category("2", "Tiles", Some(basic))
  private val plates = Category("3", "Plates", Some(basic))

  private val lookupTaxonomyParts = List(
    LegoPart("3001", "Brick 2 x 4", List(basic), 1, Set.empty),
    LegoPart("3069", "Tile 1 x 2", List(basic, tiles), 2, Set.empty),
    LegoPart("3022", "Plate 2 x 2", List(basic, plates), 3, Set.empty),
    LegoPart("3023", "Plate 1 x 2", List(basic, plates), 4, Set.empty),
    LegoPart("4000", "Doohickey", Nil, 5, Set.empty),
    LegoPart("5000", "Widget 5 x 5 Special", List(basic), 6, Set.empty)
  )
  private val lookupTaxonomyData: TaxonomyData = TaxonomyData(Set.empty, lookupTaxonomyParts)

  private val rebrickableData = Data(
    colors = Nil,
    parts = Nil,
    elements = List(
      Element(6116611L, "3069", 4, Some(3069)),  // element -> design in taxonomy
      Element(6226622L, "777", 4, Some(777))    // element -> design not in taxonomy
    ),
    sets = Nil,
    inventories = Nil,
    inventoryParts = Nil
  )

  private def deafRebrickable: ActorRef[RebrickableHolder.Command] =
    spawn(Behaviors.ignore[RebrickableHolder.Command], name = uniqueName("deaf-rebrickable"))

  private def stubRebrickable(data: Data): ActorRef[RebrickableHolder.Command] =
    spawn(Behaviors.receiveMessage[RebrickableHolder.Command] {
      case RebrickableHolder.GetData(replyTo) =>
        replyTo ! data
        Behaviors.same
      case _ => Behaviors.same
    }, name = uniqueName("stub-rebrickable"))

  private var nameCounter = 0
  private def uniqueName(prefix: String): String = {
    nameCounter += 1
    s"$prefix-${nameCounter}"
  }

  private def readTaxonomy(holder: akka.actor.typed.ActorRef[TaxonomyHolder.Command]): TaxonomyData = {
    val probe = createTestProbe[TaxonomyHolder.Response]()
    holder ! TaxonomyHolder.GetTaxonomy(probe.ref)
    probe.expectMessageType[TaxonomyHolder.TaxonomyDataResponse].taxonomyData
  }

  private def lookup(
    holder: akka.actor.typed.ActorRef[TaxonomyHolder.Command],
    requests: TaxonomyHolder.LookupPartRequest*
  ): List[TaxonomyHolder.LookupResult] = {
    val probe = createTestProbe[List[TaxonomyHolder.LookupResult]]()
    holder ! TaxonomyHolder.LookupParts(requests.toList, probe.ref)
    probe.expectMessageType[List[TaxonomyHolder.LookupResult]](5.seconds)
  }

  private def holderWith(data: TaxonomyData, rebrickable: ActorRef[RebrickableHolder.Command]): ActorRef[TaxonomyHolder.Command] = {
    val holder = spawn(TaxonomyHolder(rebrickable), name = uniqueName("holder"))
    holder ! TaxonomyHolder.SetTaxonomy(data)
    holder
  }

  "TaxonomyHolder" should {

    "return the stored taxonomy via GetTaxonomy" in {
      val holder = holderWith(taxonomyData, deafRebrickable)

      readTaxonomy(holder).parts.map(_.partNumber) should contain theSameElementsAs List("3001", "3069")
    }

    "reply with an empty taxonomy when nothing is stored" in {
      val holder = spawn(TaxonomyHolder(deafRebrickable), name = uniqueName("holder"))

      readTaxonomy(holder) shouldBe TaxonomyData(Set.empty, Nil)
    }

    "apply AugmentPart alt numbers on the next read" in {
      val holder = holderWith(taxonomyData, deafRebrickable)
      holder ! TaxonomyHolder.AugmentPart("3001", Set("3001a", "3001b"))

      val data = readTaxonomy(holder)
      data.findPart("3001a").map(_.partNumber) shouldBe Some("3001")
      data.findPart("3001b").map(_.partNumber) shouldBe Some("3001")
      data.findPart("3001").map(_.altNumbers) shouldBe Some(Set("3001a", "3001b"))
      data.findPart("3069").map(_.altNumbers) shouldBe Some(Set.empty)
    }

    "replace previous alt numbers when a part is augmented again" in {
      val holder = holderWith(taxonomyData, deafRebrickable)
      holder ! TaxonomyHolder.AugmentPart("3001", Set("3001a"))
      holder ! TaxonomyHolder.AugmentPart("3001", Set("3001c"))

      readTaxonomy(holder).findPart("3001").map(_.altNumbers) shouldBe Some(Set("3001c"))
    }

    "ignore AugmentPart when no taxonomy is stored" in {
      val holder = spawn(TaxonomyHolder(deafRebrickable), name = uniqueName("holder"))
      holder ! TaxonomyHolder.AugmentPart("3001", Set("3001a"))
      holder ! TaxonomyHolder.SetTaxonomy(taxonomyData)

      readTaxonomy(holder).findPart("3001").map(_.altNumbers) shouldBe Some(Set.empty)
    }

    "clear pending augmentations when a new taxonomy is set" in {
      val holder = holderWith(taxonomyData, deafRebrickable)
      holder ! TaxonomyHolder.AugmentPart("3001", Set("3001a"))
      holder ! TaxonomyHolder.SetTaxonomy(taxonomyData)

      readTaxonomy(holder).findPart("3001").map(_.altNumbers) shouldBe Some(Set.empty)
    }

    "ignore AugmentPart for an unknown part" in {
      val holder = holderWith(taxonomyData, deafRebrickable)
      holder ! TaxonomyHolder.AugmentPart("9999", Set("9999a"))

      val data = readTaxonomy(holder)
      data.findPart("9999a") shouldBe None
      data.parts.size shouldBe 2
    }
  }

  "TaxonomyHolder LookupParts" should {

    "resolve a part by its part number as Exact" in {
      val holder = holderWith(lookupTaxonomyData, stubRebrickable(rebrickableData))

      val result = lookup(holder, TaxonomyHolder.LookupPartRequest("3001", None, "whatever")).head

      result.via shouldBe TaxonomyHolder.Exact
      result.legoPart.map(_.partNumber) shouldBe Some("3001")
      result.categoriesGuessed shouldBe false
    }

    "resolve a part by an augmented alternate number as AltNumber" in {
      val holder = holderWith(lookupTaxonomyData, stubRebrickable(rebrickableData))
      holder ! TaxonomyHolder.AugmentPart("3001", Set("3001a"))

      val result = lookup(holder, TaxonomyHolder.LookupPartRequest("3001a", None, "whatever")).head

      result.via shouldBe TaxonomyHolder.AltNumber
      result.legoPart.map(_.partNumber) shouldBe Some("3001")
    }

    "resolve a patterned part number to its base part as Modified" in {
      val holder = holderWith(lookupTaxonomyData, stubRebrickable(rebrickableData))

      val result = lookup(holder, TaxonomyHolder.LookupPartRequest("3069pb01", None, "Tile 1 x 2 Patterned")).head

      result.via shouldBe TaxonomyHolder.Modified
      result.legoPart.map(_.partNumber) shouldBe Some("3069pb01")
      result.legoPart.map(_.name) shouldBe Some("Tile 1 x 2 (modified)")
      result.legoPart.map(_.categories) shouldBe Some(List(basic, tiles))
    }

    "resolve an unknown part number through its element ID design as Modified" in {
      val holder = holderWith(lookupTaxonomyData, stubRebrickable(rebrickableData))

      val result = lookup(holder, TaxonomyHolder.LookupPartRequest("whatever", Some("6116611"), "Tile 1 x 2")).head

      result.via shouldBe TaxonomyHolder.Modified
      result.legoPart.map(_.partNumber) shouldBe Some("3069")
      result.legoPart.map(_.name) shouldBe Some("Tile 1 x 2")
    }

    "report Miss when the element design is unknown to the taxonomy" in {
      val holder = holderWith(lookupTaxonomyData, stubRebrickable(rebrickableData))

      val result = lookup(holder, TaxonomyHolder.LookupPartRequest("whatever", Some("6226622"), "Giraffe Unicorn")).head

      result.via shouldBe TaxonomyHolder.Miss
      result.legoPart shouldBe None
      result.categoriesGuessed shouldBe false
    }

    "guess categories from a name whose words cover a taxonomy part name" in {
      val holder = holderWith(lookupTaxonomyData, stubRebrickable(rebrickableData))

      val result = lookup(holder, TaxonomyHolder.LookupPartRequest("zzz", None, "FLAT TILE 1 x 2, NO. 115")).head

      result.via shouldBe TaxonomyHolder.Guessed
      result.categoriesGuessed shouldBe true
      result.legoPart.map(_.name) shouldBe Some("Tile 1 x 2 (guessed)")
      result.legoPart.map(_.categories) shouldBe Some(List(basic, tiles))
    }

    "guess the common category prefix of the top name-search hits" in {
      val holder = holderWith(lookupTaxonomyData, stubRebrickable(rebrickableData))

      val result = lookup(holder, TaxonomyHolder.LookupPartRequest("zzz", None, "Plate 9 x 9")).head

      result.via shouldBe TaxonomyHolder.Guessed
      result.categoriesGuessed shouldBe true
      result.legoPart.map(_.categories) shouldBe Some(List(basic, plates))
      result.legoPart.map(_.name) shouldBe Some("Plate 2 x 2 (guessed)")
    }

    "report Miss when no words match the taxonomy" in {
      val holder = holderWith(lookupTaxonomyData, stubRebrickable(rebrickableData))

      val result = lookup(holder, TaxonomyHolder.LookupPartRequest("zzz", None, "Giraffe Unicorn")).head

      result.via shouldBe TaxonomyHolder.Miss
      result.legoPart shouldBe None
    }

    "fall back to fuzzy matching when an exact hit has no categories" in {
      val holder = holderWith(lookupTaxonomyData, stubRebrickable(rebrickableData))

      val result = lookup(holder, TaxonomyHolder.LookupPartRequest("4000", None, "Widget 5 x 5 Special")).head

      result.via shouldBe TaxonomyHolder.Guessed
      result.categoriesGuessed shouldBe true
      result.legoPart.map(_.categories) shouldBe Some(List(basic))
    }

    "resolve Exact even when the rebrickable holder cannot be reached" in {
      val holder = holderWith(lookupTaxonomyData, deafRebrickable)

      val result = lookup(holder, TaxonomyHolder.LookupPartRequest("3001", Some("6116611"), "whatever")).head

      result.via shouldBe TaxonomyHolder.Exact
      result.legoPart.map(_.partNumber) shouldBe Some("3001")
    }

    "reply Miss for every request when no taxonomy is stored" in {
      val holder = spawn(TaxonomyHolder(stubRebrickable(rebrickableData)), name = uniqueName("holder"))

      val results = lookup(holder, TaxonomyHolder.LookupPartRequest("3001", None, "Brick 2 x 4"))

      results.head.via shouldBe TaxonomyHolder.Miss
      results.head.legoPart shouldBe None
    }

    "preserve the request order in the response" in {
      val holder = holderWith(lookupTaxonomyData, stubRebrickable(rebrickableData))

      val results = lookup(
        holder,
        TaxonomyHolder.LookupPartRequest("zzz", None, "Giraffe Unicorn"),
        TaxonomyHolder.LookupPartRequest("3001", None, "whatever"),
        TaxonomyHolder.LookupPartRequest("zzz", None, "FLAT TILE 1 x 2, NO. 115")
      )

      results.map(_.via) shouldBe List(TaxonomyHolder.Miss, TaxonomyHolder.Exact, TaxonomyHolder.Guessed)
    }
  }
}
