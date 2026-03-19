package com.wolfskeep

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import org.scalatest.wordspec.AnyWordSpecLike

import java.io.File

class DiskCacheSpec extends ScalaTestWithActorTestKit with AnyWordSpecLike {
  "A DiskCache" must {
    "insert and retrieve a value" in {
      val cache = spawn(DiskCache())
      val probe = createTestProbe[DiskCache.Response]()

      cache ! DiskCache.Insert("key1", "value1")
      cache ! DiskCache.Fetch("key1", probe.ref)
      val fetched = probe.expectMessageType[DiskCache.FetchResult]
      fetched.key should ===("key1")
      fetched.value should ===("value1")
      // insertedAt already checked in next test
    }

    "insert and retrieve insertion time" in {
      val cache = spawn(DiskCache())
      val probe = createTestProbe[DiskCache.Response]()

      val before = System.currentTimeMillis()
      cache ! DiskCache.Insert("key2", "value2")
      cache ! DiskCache.Fetch("key2", probe.ref)
      val response = probe.expectMessageType[DiskCache.FetchResult]
      val after = System.currentTimeMillis()

      response.insertedAt should be >= before
      response.insertedAt should be <= after
    }

    "return NotFound for non-existent key" in {
      val cache = spawn(DiskCache())
      val probe = createTestProbe[DiskCache.Response]()

      cache ! DiskCache.Fetch("nonexistent", probe.ref)
      probe.expectMessage(DiskCache.NotFound("nonexistent"))
    }
  }
}