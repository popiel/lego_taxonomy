package com.wolfskeep

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import org.scalatest.wordspec.AnyWordSpecLike
import akka.actor.typed.ActorRef
import akka.actor.typed.scaladsl.Behaviors

import com.wolfskeep.rebrickable.{Data, RebrickableHolder}

import java.nio.file.{Files, Path}
import scala.concurrent.duration._

class TaxonomySortMainSpec extends ScalaTestWithActorTestKit with AnyWordSpecLike {

  private val basic = Category("1", "Basic", None)

  private val stubRebrickable: ActorRef[RebrickableHolder.Command] =
    spawn(Behaviors.receiveMessage[RebrickableHolder.Command] {
      case RebrickableHolder.GetData(replyTo) =>
        replyTo ! Data(Nil, Nil, Nil, Nil, Nil, Nil)
        Behaviors.same
      case _ => Behaviors.same
    }, "stub-rebrickable")

  private val taxonomyHolder: ActorRef[TaxonomyHolder.Command] =
    spawn(TaxonomyHolder(stubRebrickable), "taxonomy-holder")

  taxonomyHolder ! TaxonomyHolder.SetTaxonomy(TaxonomyData(
    Set.empty,
    List(LegoPart("3001", "Brick 2 x 4", List(basic), 1, Set.empty))
  ))

  private val csvContent = """BLItemNo,ElementId,LdrawId,PartName,BLColorId,LDrawColorId,ColorName,ColorCategory,Qty,Weight
3001,300123,3001.dat,Brick 2 x 4,7,1,Blue,Solid colors,2,2.32
99999,999991,99999.dat,Giraffe Unicorn,5,4,Red,Solid colors,1,1.00
"""

  implicit val scheduler: akka.actor.typed.Scheduler = system.scheduler

  "processInventories" should {

    "resolve parts through the taxonomy holder service and write a sorted CSV" in {
      val tempDir = Files.createTempDirectory("taxonomy-batch")
      val inputFile = tempDir.resolve("inventory.csv")
      Files.write(inputFile, csvContent.getBytes("UTF-8"))

      implicit val timeout: akka.util.Timeout = akka.util.Timeout(10.seconds)
      TaxonomySortMain.processInventories(taxonomyHolder, Array(inputFile.toString))

      val output = new String(Files.readAllBytes(tempDir.resolve("inventory-sorted.csv")), "UTF-8")
      val lines = output.split("\n").toList
      lines.head should === ("quantity,color,partNumber_input,name_input,partNumber_taxonomy,name_taxonomy,category,category2,category3,category4")

      // matched part carries the taxonomy columns; unmatched part lands at the end
      val matchedRow = lines.tail.find(_.startsWith("2,Blue,3001")).get
      matchedRow should include (",Brick 2 x 4,")
      matchedRow should include (",Basic,")

      val unmatchedRow = lines.tail.find(_.startsWith("1,Red,99999")).get
      unmatchedRow should include (",Giraffe Unicorn,,,,,,")
      lines.tail.last should startWith ("1,Red,99999")

      Files.deleteIfExists(tempDir.resolve("inventory-sorted.csv"))
      Files.deleteIfExists(inputFile)
      Files.deleteIfExists(tempDir)
    }
  }
}
