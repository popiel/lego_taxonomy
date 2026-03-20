package com.wolfskeep

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.BeforeAndAfterAll

import scala.concurrent.duration._
import java.io.File

class BricklinkActorSpec extends ScalaTestWithActorTestKit with AnyWordSpecLike with BeforeAndAfterAll {
  private val credentialsFile = new File("bricklink_ids.json")
  private val bricklinkCacheDir = new File(".cache/bricklink")

  override def beforeAll(): Unit = {
    super.beforeAll()
    if (bricklinkCacheDir.exists()) {
      bricklinkCacheDir.listFiles().foreach(_.delete())
    }
  }

  def skipIfNoCredentials(): Unit = {
    assume(credentialsFile.exists(), s"Bricklink credentials file not found at ${credentialsFile.getAbsolutePath}")
  }

  "BricklinkActor" should {
    "get item 3001 successfully" in {
      skipIfNoCredentials()
      val cache = spawn(DiskCache())
      val bricklinkActor = spawn(BricklinkActor(cache))
      val probe = createTestProbe[BricklinkActor.Response]()

      bricklinkActor ! BricklinkActor.GetItem("part", "3001", probe.ref)

      val response = probe.expectMessageType[BricklinkActor.Response](30.seconds)
      response match {
        case BricklinkActor.ItemResponse(item) =>
          item.no should ===("3001")
          item.itemType should ===("PART")
          item.categoryId should be > 0
        case BricklinkActor.Failed(message) =>
          if (message.contains("VERSION_REJECTED") || message.contains("SIGNATURE_INVALID")) {
            cancel(s"BrickLink authentication failed: $message")
          } else {
            fail(s"BrickLink API error: $message")
          }
      }
    }

    "return item with name and image URL" in {
      skipIfNoCredentials()
      val cache = spawn(DiskCache())
      val bricklinkActor = spawn(BricklinkActor(cache))
      val probe = createTestProbe[BricklinkActor.Response]()

      bricklinkActor ! BricklinkActor.GetItem("part", "3001", probe.ref)

      val response = probe.expectMessageType[BricklinkActor.Response](30.seconds)
      response match {
        case BricklinkActor.ItemResponse(item) =>
          item.name should not be empty
          item.imageUrl should not be empty
        case BricklinkActor.Failed(message) =>
          if (message.contains("VERSION_REJECTED") || message.contains("SIGNATURE_INVALID")) {
            cancel(s"BrickLink authentication failed: $message")
          } else {
            fail(s"BrickLink API error: $message")
          }
      }
    }

    "get item number by element ID 6331694" in {
      skipIfNoCredentials()
      val cache = spawn(DiskCache())
      val bricklinkActor = spawn(BricklinkActor(cache))
      val probe = createTestProbe[BricklinkActor.Response]()

      bricklinkActor ! BricklinkActor.GetItemNumberByElementId("6331694", probe.ref)

      val response = probe.expectMessageType[BricklinkActor.Response](30.seconds)
      response match {
        case BricklinkActor.ItemMappingResponse(itemNo, itemType) =>
          itemNo should startWith("30350")
          itemType should be("PART")
        case BricklinkActor.Failed(message) =>
          if (message.contains("VERSION_REJECTED") || message.contains("SIGNATURE_INVALID")) {
            cancel(s"BrickLink authentication failed: $message")
          } else {
            fail(s"BrickLink API error: $message")
          }
      }
    }
  }
}
