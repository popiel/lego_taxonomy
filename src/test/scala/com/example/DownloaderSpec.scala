package com.example

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.model.headers._
import akka.http.scaladsl.server.Route
import org.scalatest.wordspec.AnyWordSpecLike
import com.example.Downloader.{Downloaded, Failed, NotChanged}

import scala.concurrent.Await
import scala.concurrent.duration._

class DownloaderSpec extends ScalaTestWithActorTestKit with AnyWordSpecLike {
  "A Downloader" must {
    "return content when the server replies 200" in {
      // set up a small HTTP server
      val route: Route = path("hello") {
        get {
          complete(HttpEntity("world"))
        }
      }
      val binding = Await.result(Http()(testKit.system.classicSystem).newServerAt("localhost", 0).bind(route), 3.seconds)
      val port = binding.localAddress.getPort

      try {
        val probe = createTestProbe[Downloader.Response]()
        val downloader = spawn(Downloader())
        downloader ! Downloader.Fetch(s"http://localhost:$port/hello", probe.ref)
        probe.expectMessage(Downloaded("http://localhost:%d/hello".format(port), "world"))
      } finally {
        Await.result(binding.unbind(), 3.seconds)
      }
    }

    "fail when the server returns an error status" in {
      val route: Route = path("bad") {
        get {
          complete(StatusCodes.InternalServerError)
        }
      }
      val binding = Await.result(Http()(testKit.system.classicSystem).newServerAt("localhost", 0).bind(route), 3.seconds)
      val port = binding.localAddress.getPort

      try {
        val probe = createTestProbe[Downloader.Response]()
        val downloader = spawn(Downloader())
        downloader ! Downloader.Fetch(s"http://localhost:$port/bad", probe.ref)
        val msg = probe.expectMessageType[Downloader.Failed]
        msg.url should ===(s"http://localhost:$port/bad")
        msg.reason should include("HTTP")
      } finally {
        Await.result(binding.unbind(), 3.seconds)
      }
    }

    "fail when there is no server listening" in {
      // choose a port that is unlikely to be used (0 means ephemeral, but we want closed)
      val badPort = 9 // discard protocol port; likely closed
      val probe = createTestProbe[Downloader.Response]()
      val downloader = spawn(Downloader())
      downloader ! Downloader.Fetch(s"http://localhost:$badPort/anything", probe.ref)
      val msg = probe.expectMessageType[Downloader.Failed](5.seconds)
      msg.url should ===(s"http://localhost:$badPort/anything")
      msg.reason should not be empty
    }

    "respect If-Modified-Since and fetch when modified" in {
      // server last-modified timestamp
      val lastMod = akka.http.scaladsl.model.DateTime(2020, 1, 1, 0, 0)
      val route: Route = path("check") {
        get {
          extractRequest { req =>
            req.header[`If-Modified-Since`] match {
              case Some(`If-Modified-Since`(date)) if date == lastMod =>
                complete(StatusCodes.NotModified)
              case _ =>
                respondWithHeader(akka.http.scaladsl.model.headers.`Last-Modified`(lastMod)) {
                  complete(HttpEntity("fresh"))
                }
            }
          }
        }
      }
      val binding = Await.result(Http()(testKit.system.classicSystem).newServerAt("localhost", 0).bind(route), 3.seconds)
      val port = binding.localAddress.getPort
      try {
        val probe = createTestProbe[Downloader.Response]()
        val downloader = spawn(Downloader())
        // request with an older date -> should download
        val oldDate = akka.http.scaladsl.model.DateTime(2019, 1, 1, 0, 0)
        downloader ! Downloader.Fetch(s"http://localhost:$port/check", probe.ref, since = Some(oldDate))
        probe.expectMessage(Downloaded(s"http://localhost:$port/check", "fresh"))

        // request with the same date -> not changed
        val probe2 = createTestProbe[Downloader.Response]()
        downloader ! Downloader.Fetch(s"http://localhost:$port/check", probe2.ref, since = Some(lastMod))
        probe2.expectMessage(NotChanged(s"http://localhost:$port/check"))
      } finally {
        Await.result(binding.unbind(), 3.seconds)
      }
    }

    "retry request when server returns 429" in {
      var attempt = 0
      val route: Route = path("retry") {
        get {
          attempt += 1
          if (attempt == 1) {
            complete(StatusCodes.TooManyRequests)
          } else {
            complete(HttpEntity("success after retry"))
          }
        }
      }
      val binding = Await.result(Http()(testKit.system.classicSystem).newServerAt("localhost", 0).bind(route), 3.seconds)
      val port = binding.localAddress.getPort

      try {
        val probe = createTestProbe[Downloader.Response]()
        val downloader = spawn(Downloader(500.millis))
        downloader ! Downloader.Fetch(s"http://localhost:$port/retry", probe.ref)
        probe.expectMessage(Downloaded(s"http://localhost:$port/retry", "success after retry"))
      } finally {
        Await.result(binding.unbind(), 3.seconds)
      }
      attempt should ===(2) // ensure that the retry actually happened
    }

    "fetch inventory from brickset.com and return CSV content" in {
      val probe = createTestProbe[Downloader.Response]()
      val downloader = spawn(Downloader())
      downloader ! Downloader.Fetch("https://brickset.com/exportscripts/inventory/21002-1", probe.ref)
      val msg = probe.expectMessageType[Downloader.Downloaded](30.seconds)
      val coloredParts = new CsvReader().readColoredPartsFromString(msg.content)
      coloredParts.length should be > 2
    }

    "return no parts when fetching set 21002 without -1 suffix" in {
      val probe = createTestProbe[Downloader.Response]()
      val downloader = spawn(Downloader())
      downloader ! Downloader.Fetch("https://brickset.com/exportscripts/inventory/21002", probe.ref)
      val msg = probe.expectMessageType[Downloader.Downloaded](30.seconds)
      val coloredParts = new CsvReader().readColoredPartsFromString(msg.content)
      coloredParts.length should be(0)
    }
  }
}
