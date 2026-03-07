package com.example

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import org.scalatest.wordspec.AnyWordSpecLike

import java.io.File

class CacheActorSpec extends ScalaTestWithActorTestKit with AnyWordSpecLike {
  "A CacheActor" must {
    "insert and retrieve a value" in {
      val cache = spawn(CacheActor())
      val probe = createTestProbe[CacheActor.Response]()

      cache ! CacheActor.Insert("key1", "value1")
      cache ! CacheActor.GetValue("key1", probe.ref)
      probe.expectMessage(CacheActor.Value("value1"))
    }

    "insert and retrieve insertion time" in {
      val cache = spawn(CacheActor())
      val probe = createTestProbe[CacheActor.Response]()

      val before = System.currentTimeMillis()
      cache ! CacheActor.Insert("key2", "value2")
      cache ! CacheActor.GetInsertionTime("key2", probe.ref)
      val response = probe.expectMessageType[CacheActor.InsertionTime]
      val after = System.currentTimeMillis()

      response.time should be >= before
      response.time should be <= after
    }

    "return NotFound for non-existent key" in {
      val cache = spawn(CacheActor())
      val probe = createTestProbe[CacheActor.Response]()

      cache ! CacheActor.GetValue("nonexistent", probe.ref)
      probe.expectMessage(CacheActor.NotFound("nonexistent"))

      cache ! CacheActor.GetInsertionTime("nonexistent", probe.ref)
      probe.expectMessage(CacheActor.NotFound("nonexistent"))
    }
  }
}