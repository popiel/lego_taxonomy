package com.wolfskeep

import akka.actor.typed.ActorRef
import akka.actor.typed.ActorSystem
import akka.actor.typed.Behavior
import akka.actor.typed.Props
import akka.actor.typed.SpawnProtocol
import akka.actor.typed.scaladsl.AskPattern._
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.matchers.should.Matchers
import com.wolfskeep.rebrickable._

import akka.util.Timeout
import scala.concurrent.duration._
import scala.concurrent.Await

class RenderingGoldenSpec extends AnyWordSpecLike with Matchers with ScalatestRouteTest {

  implicit val timeout: Timeout = 3.seconds

  val typedSystem: ActorSystem[SpawnProtocol.Command] = ActorSystem(SpawnProtocol(), "rendering-golden")
  implicit val scheduler: akka.actor.typed.Scheduler = typedSystem.scheduler

  def spawnActor[T](behavior: Behavior[T], name: String): ActorRef[T] =
    Await.result(
      typedSystem.ask[ActorRef[T]](replyTo => SpawnProtocol.Spawn(
        behavior = behavior,
        name = name,
        props = Props.empty,
        replyTo = replyTo
      )),
      3.seconds
    )

  private val curve = Category("1", "Curve", None)
  private val round = Category("2", "Round", Some(curve))
  private val tile = Category("3", "Tile", Some(round))
  private val basic = Category("4", "Basic", None)

  private val taxonomyParts = List(
    LegoPart(
      "3001", "Brick 2 x 4", List(basic), 1, Set.empty,
      Some("https://brickarchitect.com/label/partqrcode.php?part_num=3001"), Some("120"), Some("90")
    ),
    LegoPart(
      "3024", "Plate 1 x 1", List(basic), 2, Set.empty,
      Some("https://brickarchitect.com/label/partqrcode.php?part_num=3024"), None, None
    ),
    LegoPart(
      "98138", "Tile Round 1 x 1", List(curve, round, tile), 3, Set.empty,
      Some("https://brickarchitect.com/label/partqrcode.php?part_num=98138"), Some("100"), Some("100")
    )
  )

  private val rebrickableData: Data = Data(
    colors = List(
      Color(1, "Blue", "0000ff", false, 0, 0, 0, 0),
      Color(2, "Red", "ff0000", false, 0, 0, 0, 0)
    ),
    parts = List(
      Part("3001", "Brick 2 x 4", 1, "Plastic"),
      Part("3024", "Plate 1 x 1", 1, "Plastic"),
      Part("98138pr0035", "Tile Round 1 x 1 with Soda Can Tab, Ring Pull Print", 1, "Plastic"),
      Part("99999", "Zorble Magnifier", 1, "Plastic")
    ),
    elements = List(
      Element(6116611L, "98138pr0035", 1, Some(98138))
    ),
    sets = List(
      RebrickableSet("21321-1", "Treehouse", 2020, 1, 2, None)
    ),
    inventories = List(
      Inventory(1, 1, "21321-1")
    ),
    inventoryParts = List(
      InventoryPart(1, "3001", 1, 2, false, None),
      InventoryPart(1, "3024", 2, 1, false, None),
      InventoryPart(1, "98138pr0035", 1, 4, false, None),
      InventoryPart(1, "99999", 2, 1, false, None)
    )
  )

  val rebrickableDataActor: ActorRef[RebrickableHolder.Command] = spawnActor(
    Behaviors.receiveMessage[RebrickableHolder.Command] {
      case RebrickableHolder.GetData(replyTo) =>
        replyTo ! rebrickableData
        Behaviors.same
      case _ =>
        Behaviors.same
    },
    "rebrickable-data"
  )

  val taxonomyHolder: ActorRef[TaxonomyHolder.Command] =
    spawnActor(TaxonomyHolder(rebrickableDataActor), "taxonomy-holder")

  taxonomyHolder ! TaxonomyHolder.SetTaxonomy(TaxonomyData(Set.empty, taxonomyParts))

  Thread.sleep(100)

  val partsProcessor: ActorRef[PartsProcessor.Command] =
    spawnActor(PartsProcessor(taxonomyHolder), "parts-processor")

  val imageResolver: ActorRef[ImageResolver.Command] = spawnActor(
    Behaviors.receiveMessage[ImageResolver.Command] {
      case ImageResolver.GetLdrawImage(_, _, replyTo) =>
        replyTo ! ImageResolver.LdrawImageReady(Array[Byte](1, 2, 3))
        Behaviors.same
      case ImageResolver.GetBricksetImageUrl(_, _, replyTo) =>
        replyTo ! ImageResolver.BricksetImageResolved("https://example.com/98138pr0035.jpg")
        Behaviors.same
    },
    "image-resolver"
  )

  val route: Route =
    Routes.all(typedSystem, partsProcessor, rebrickableDataActor, imageResolver)

  "Rendering through the real TaxonomyHolder-PartsProcessor-Routes chain" must {

    "redirect a submitted set number to its bookmarkable URL" in {
      Post("/parts-sorter", FormData("setNumber" -> "21321-1")) ~> route ~> check {
        status should ===(StatusCodes.SeeOther)
        header("Location").map(_.value) should ===(Some("/parts-sorter?setNumber=21321-1"))
      }
    }

    "render the exact image markup per lookup via for set 21321-1 from its URL" in {
      Get("/parts-sorter?setNumber=21321-1") ~> route ~> check {
        status should ===(StatusCodes.OK)
        val responseBody = entityAs[String]

        responseBody should include(
          """<td data-col-id="image"><img src="https://brickarchitect.com/label/partqrcode.php?part_num=3001" width="120" height="90" /></td>""")

        responseBody should include(
          """<td data-col-id="image"><img src="https://brickarchitect.com/label/partqrcode.php?part_num=3024" style="max-width: 120px" /></td>""")

        responseBody should include(
          """<td data-col-id="image"><img alt="" style="max-width: 120px" data-image-ldraw="/part_images/1/98138pr0035.png" data-image-brickset="/part_images/brickset/98138pr0035?element=6116611" /></td>""")

        responseBody should include(
          """<td data-col-id="image"></td>""")

        responseBody should include(
          """value="21321-1"""")

        responseBody.split("<img").length - 1 should be(3)
        responseBody.split("data-image-ldraw").length - 1 should be(1)
        responseBody.split("""<td data-col-id="image"></td>""").length - 1 should be(1)
      }
    }
  }
}
