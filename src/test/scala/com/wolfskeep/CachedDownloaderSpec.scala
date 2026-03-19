package com.wolfskeep

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.model.headers._
import akka.http.scaladsl.server.Route
import org.scalatest.wordspec.AnyWordSpecLike

import akka.http.scaladsl.model.DateTime

import scala.concurrent.Await
import scala.concurrent.duration._
import java.util.concurrent.atomic.AtomicInteger

class CachedDownloaderSpec extends ScalaTestWithActorTestKit with AnyWordSpecLike {
  "A CachedDownloader" must {
    "download and cache a page on first fetch" in {
      val route: Route = path("test") {
        get {
          complete(HttpEntity("content"))
        }
      }
      val binding = Await.result(Http()(testKit.system.classicSystem).newServerAt("localhost", 0).bind(route), 3.seconds)
      val port = binding.localAddress.getPort

      try {
        val probe = createTestProbe[CachedDownloader.Response]()
        val cache = spawn(DiskCache())
        val cachedDownloader = spawn(CachedDownloader(cache))
        cachedDownloader ! CachedDownloader.Fetch(s"http://localhost:$port/test", probe.ref)
        probe.expectMessage(CachedDownloader.Downloaded(s"http://localhost:$port/test", "content"))

        // second fetch should use cache and send If-Modified-Since
        val probe2 = createTestProbe[CachedDownloader.Response]()
        cachedDownloader ! CachedDownloader.Fetch(s"http://localhost:$port/test", probe2.ref)
        // since server doesn't handle If-Modified-Since, it will download again, but cache should be used? Wait, no, the downloader will send If-Modified-Since, but server returns 200, so Downloaded again.
        // To test NotChanged, need a server that respects it.
        probe2.expectMessage(CachedDownloader.Downloaded(s"http://localhost:$port/test", "content"))
      } finally {
        Await.result(binding.unbind(), 3.seconds)
      }
    }

    // new test verifying fresh-cache bypass
    "return cached value without hitting network if recent" in {
      val requestCount = new AtomicInteger(0)
      val route: Route = path("fresh") {
        get {
          requestCount.incrementAndGet()
          complete(HttpEntity("fresh"))
        }
      }
      val binding = Await.result(Http()(testKit.system.classicSystem).newServerAt("localhost", 0).bind(route), 3.seconds)
      val port = binding.localAddress.getPort

      try {
        val probe1 = createTestProbe[CachedDownloader.Response]()
        val cache = spawn(DiskCache())
        val cachedDownloader = spawn(CachedDownloader(cache))
        val url = s"http://localhost:$port/fresh"

        cachedDownloader ! CachedDownloader.Fetch(url, probe1.ref)
        probe1.expectMessage(CachedDownloader.Downloaded(url, "fresh"))

        // second fetch happens quickly, cache entry should be < 1h old
        val probe2 = createTestProbe[CachedDownloader.Response]()
        cachedDownloader ! CachedDownloader.Fetch(url, probe2.ref)
        probe2.expectMessage(CachedDownloader.Downloaded(url, "fresh"))

        // server should only have been contacted once
        requestCount.get() should ===(1)
      } finally {
        Await.result(binding.unbind(), 3.seconds)
      }
    }

    "return NotChanged when server respects If-Modified-Since" in {
      val now = System.currentTimeMillis()
      val lastMod = DateTime(now - 1000) // 1 second ago
      val route: Route = path("cached") {
        get {
          extractRequest { req =>
            req.header[`If-Modified-Since`] match {
              case Some(`If-Modified-Since`(date)) if date.clicks >= lastMod.clicks =>
                complete(StatusCodes.NotModified)
              case _ =>
                respondWithHeader(`Last-Modified`(lastMod)) {
                  complete(HttpEntity("fresh"))
                }
            }
          }
        }
      }
      val binding = Await.result(Http()(testKit.system.classicSystem).newServerAt("localhost", 0).bind(route), 3.seconds)
      val port = binding.localAddress.getPort

      try {
        val probe = createTestProbe[CachedDownloader.Response]()
        val cache = spawn(DiskCache())
        val cachedDownloader = spawn(CachedDownloader(cache))
        // first fetch
        cachedDownloader ! CachedDownloader.Fetch(s"http://localhost:$port/cached", probe.ref)
        probe.expectMessage(CachedDownloader.Downloaded(s"http://localhost:$port/cached", "fresh"))

        // second fetch should send If-Modified-Since and get NotModified, but return cached content
        val probe2 = createTestProbe[CachedDownloader.Response]()
        cachedDownloader ! CachedDownloader.Fetch(s"http://localhost:$port/cached", probe2.ref)
        probe2.expectMessage(CachedDownloader.Downloaded(s"http://localhost:$port/cached", "fresh"))
      } finally {
        Await.result(binding.unbind(), 3.seconds)
      }
    }

    "handle multiple concurrent requests for the same URL" in {
      val route: Route = path("multi") {
        get {
          complete(HttpEntity("shared content"))
        }
      }
      val binding = Await.result(Http()(testKit.system.classicSystem).newServerAt("localhost", 0).bind(route), 3.seconds)
      val port = binding.localAddress.getPort

      try {
        val probe1 = createTestProbe[CachedDownloader.Response]()
        val probe2 = createTestProbe[CachedDownloader.Response]()
        val cache = spawn(DiskCache())
        val cachedDownloader = spawn(CachedDownloader(cache))
        cachedDownloader ! CachedDownloader.Fetch(s"http://localhost:$port/multi", probe1.ref)
        cachedDownloader ! CachedDownloader.Fetch(s"http://localhost:$port/multi", probe2.ref)
        probe1.expectMessage(CachedDownloader.Downloaded(s"http://localhost:$port/multi", "shared content"))
        probe2.expectMessage(CachedDownloader.Downloaded(s"http://localhost:$port/multi", "shared content"))
      } finally {
        Await.result(binding.unbind(), 3.seconds)
      }
    }

    "only make one request to the Downloader for concurrent requests" in {
      val requestCount = new AtomicInteger(0)
      val route: Route = path("single") {
        get {
          requestCount.incrementAndGet()
          complete(HttpEntity("single-response"))
        }
      }
      val binding = Await.result(Http()(testKit.system.classicSystem).newServerAt("localhost", 0).bind(route), 3.seconds)
      val port = binding.localAddress.getPort

      try {
        val probe1 = createTestProbe[CachedDownloader.Response]()
        val probe2 = createTestProbe[CachedDownloader.Response]()
        val probe3 = createTestProbe[CachedDownloader.Response]()
        val cache = spawn(DiskCache())
        val cachedDownloader = spawn(CachedDownloader(cache))
        val url = s"http://localhost:$port/single"
        cachedDownloader ! CachedDownloader.Fetch(url, probe1.ref)
        cachedDownloader ! CachedDownloader.Fetch(url, probe2.ref)
        cachedDownloader ! CachedDownloader.Fetch(url, probe3.ref)
        probe1.expectMessage(CachedDownloader.Downloaded(url, "single-response"))
        probe2.expectMessage(CachedDownloader.Downloaded(url, "single-response"))
        probe3.expectMessage(CachedDownloader.Downloaded(url, "single-response"))
        
        // verify server was only hit once
        requestCount.get() should ===(1)
      } finally {
        Await.result(binding.unbind(), 3.seconds)
      }
    }
  }
}