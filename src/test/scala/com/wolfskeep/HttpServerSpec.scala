package com.wolfskeep

import akka.actor.typed.ActorRef
import akka.actor.typed.ActorSystem
import akka.actor.typed.Props
import akka.actor.typed.SpawnProtocol
import akka.actor.typed.scaladsl.AskPattern._
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.matchers.should.Matchers
import com.wolfskeep.rebrickable.{Data, RebrickableDataActor}

import akka.util.Timeout
import scala.concurrent.duration._
import scala.concurrent.Await

class HttpServerSpec extends AnyWordSpecLike with Matchers with ScalatestRouteTest {
  
  implicit val timeout: Timeout = 3.seconds
  
  val typedSystem: ActorSystem[SpawnProtocol.Command] = ActorSystem(SpawnProtocol(), "test")
  implicit val scheduler: akka.actor.typed.Scheduler = typedSystem.scheduler
  
  val partsProcessor: ActorRef[PartsProcessor.Command] = Await.result(
    typedSystem.ask[ActorRef[PartsProcessor.Command]](replyTo => SpawnProtocol.Spawn(
      behavior = Behaviors.receiveMessage[PartsProcessor.Command] {
        case PartsProcessor.ProcessParts(coloredParts, replyTo) =>
          val matchedParts = coloredParts.map { cp =>
            MatchedPart(cp, None)
          }
          replyTo ! PartsProcessor.ProcessedParts(matchedParts)
          Behaviors.same
      },
      name = "partsProcessor",
      props = Props.empty,
      replyTo = replyTo
    )),
    3.seconds
  )

  val rebrickableDataActor: ActorRef[RebrickableDataActor.Command] = Await.result(
    typedSystem.ask[ActorRef[RebrickableDataActor.Command]](replyTo => SpawnProtocol.Spawn(
      behavior = Behaviors.receiveMessage[RebrickableDataActor.Command] {
        case RebrickableDataActor.GetData(replyTo) =>
          replyTo ! Data(Nil, Nil, Nil, Nil, Nil, Nil)
          Behaviors.same
        case _ =>
          Behaviors.same
      },
      name = "rebrickableDataActor",
      props = Props.empty,
      replyTo = replyTo
    )),
    3.seconds
  )

  val route: Route = Routes.all(typedSystem, partsProcessor, rebrickableDataActor)

  "HttpServer routes" must {
    
    "redirect root to parts-sorter.html" in {
      Get("/") ~> route ~> check {
        status should ===(StatusCodes.Found)
        header("Location").map(_.value) should ===(Some("/parts-sorter.html"))
      }
    }
    
    "serve parts-sorter.html with description and form" in {
      Get("/parts-sorter.html") ~> route ~> check {
        status should ===(StatusCodes.OK)
        contentType should ===(ContentTypes.`text/html(UTF-8)`)
        val responseBody = entityAs[String]
        responseBody should include("LEGO Parts Sorter")
        responseBody should include("Tom Alphin")
        responseBody should include("brickarchitect.com")
        responseBody should include("""<input type="text" name="setNumber" id="setNumber" placeholder="e.g., 21321-1">""")
        responseBody should include("""<input type="file" name="inputFile" id="inputFile" accept=".csv,.io">""")
        responseBody should include("""<form method="POST" action="/parts-sorter.html" enctype="multipart/form-data" id="uploadForm">""")
      }
    }
    
    "handle CSV upload and display results with file name" in {
      val csvContent = """BLItemNo,ElementId,LdrawId,PartName,BLColorId,LDrawColorId,ColorName,ColorCategory,Qty,Weight
3001,300123,3001.dat,Brick 2 x 4,7,1,Blue,Solid Colors,2,2.32
3001,300121,3001.dat,Brick 2 x 4,5,4,Red,Solid Colors,1,2.32
"""
      
      val formData = Multipart.FormData(
        Multipart.FormData.BodyPart(
          "inputFile",
          HttpEntity(ContentTypes.`text/csv(UTF-8)`, csvContent),
          Map("filename" -> "test.csv")
        )
      )
      
      Post("/parts-sorter.html", formData) ~> route ~> check {
        status should ===(StatusCodes.OK)
        contentType should ===(ContentTypes.`text/html(UTF-8)`)
        val responseBody = entityAs[String]
        responseBody should include("Uploaded file: test.csv")
        responseBody should include("<table>")
        responseBody should include("<th>quantity</th>")
        responseBody should include("<th>input partNumber</th>")
        responseBody should include("<td>2</td>")
        responseBody should include("<td>Blue</td>")
        responseBody should include("<td>3001</td>")
      }
    }
    
    "handle .io file upload and include part names from model2.ldr" in {
      val ioFile = new java.io.File("src/test/resources/simple.io")
      val ioBytes = {
        val fis = new java.io.FileInputStream(ioFile)
        val bytes = Array.ofDim[Byte](ioFile.length.toInt)
        fis.read(bytes)
        fis.close()
        bytes
      }
      
      val formData = Multipart.FormData(
        Multipart.FormData.BodyPart(
          "inputFile",
          HttpEntity(MediaTypes.`application/octet-stream`, ioBytes),
          Map("filename" -> "simple.io")
        )
      )
      
      Post("/parts-sorter.html", formData) ~> route ~> check {
        status should ===(StatusCodes.OK)
        contentType should ===(ContentTypes.`text/html(UTF-8)`)
        val responseBody = entityAs[String]
        responseBody should include("Uploaded file: simple.io")
        responseBody should include("<table>")
        responseBody should include("Brick 2 x 4")
        responseBody should include("Brick 1 x 4")
      }
    }
    
    "handle request without file part" in {
      Post("/parts-sorter.html") ~> route ~> check {
        handled shouldBe false
      }
    }
  }
}
