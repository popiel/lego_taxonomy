package com.example

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import org.scalatest.wordspec.AnyWordSpecLike

import java.io.File

class DiskCacheSpec extends ScalaTestWithActorTestKit with AnyWordSpecLike {
  "A DiskCache" must {
    "insert and retrieve a value" in {
      val cache = spawn(DiskCache())
      val probe = createTestProbe[DiskCache.Response]()

      cache ! DiskCache.Insert("key1", "value1")
      cache ! DiskCache.GetValue("key1", probe.ref)
      probe.expectMessage(DiskCache.Value("value1"))
    }

    "insert and retrieve insertion time" in {
      val cache = spawn(DiskCache())
      val probe = createTestProbe[DiskCache.Response]()

      val before = System.currentTimeMillis()
      cache ! DiskCache.Insert("key2", "value2")
      cache ! DiskCache.GetInsertionTime("key2", probe.ref)
      val response = probe.expectMessageType[DiskCache.InsertionTime]
      val after = System.currentTimeMillis()

      response.time should be >= before
      response.time should be <= after
    }

    "return NotFound for non-existent key" in {
      val cache = spawn(DiskCache())
      val probe = createTestProbe[DiskCache.Response]()

      cache ! DiskCache.GetValue("nonexistent", probe.ref)
      probe.expectMessage(DiskCache.NotFound("nonexistent"))

      cache ! DiskCache.GetInsertionTime("nonexistent", probe.ref)
      probe.expectMessage(DiskCache.NotFound("nonexistent"))
    }
  }
}