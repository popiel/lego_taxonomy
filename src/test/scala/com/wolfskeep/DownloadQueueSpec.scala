package com.wolfskeep

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import org.scalatest.wordspec.AnyWordSpecLike
import akka.actor.typed.ActorRef

import scala.concurrent.duration._

class DownloadQueueSpec extends ScalaTestWithActorTestKit with AnyWordSpecLike {
  "A DownloadQueue" must {
    "pass through Downloaded responses" in {
      val downloaderProbe = createTestProbe[Downloader.Command]("downloader")
      val queue = spawn(DownloadQueue(concurrencyLimit = 2, downloaderProbe.ref))
      val replyProbe = createTestProbe[Downloader.Response]("reply")
      
      queue ! DownloadQueue.Fetch("http://example.com/test", replyProbe.ref)
      
      val fetchMsg = downloaderProbe.expectMessageType[Downloader.Fetch]
      fetchMsg.url should ===("http://example.com/test")
      fetchMsg.replyTo ! Downloader.Downloaded("http://example.com/test", "content")
      
      replyProbe.expectMessage(Downloader.Downloaded("http://example.com/test", "content"))
    }
    
    "pass through NotChanged responses" in {
      val downloaderProbe = createTestProbe[Downloader.Command]("downloader")
      val queue = spawn(DownloadQueue(concurrencyLimit = 2, downloaderProbe.ref))
      val replyProbe = createTestProbe[Downloader.Response]("reply")
      
      queue ! DownloadQueue.Fetch("http://example.com/test", replyProbe.ref)
      
      val fetchMsg = downloaderProbe.expectMessageType[Downloader.Fetch]
      fetchMsg.replyTo ! Downloader.NotChanged("http://example.com/test")
      
      replyProbe.expectMessage(Downloader.NotChanged("http://example.com/test"))
    }
    
    "pass through Failed responses" in {
      val downloaderProbe = createTestProbe[Downloader.Command]("downloader")
      val queue = spawn(DownloadQueue(concurrencyLimit = 2, downloaderProbe.ref))
      val replyProbe = createTestProbe[Downloader.Response]("reply")
      
      queue ! DownloadQueue.Fetch("http://example.com/test", replyProbe.ref)
      
      val fetchMsg = downloaderProbe.expectMessageType[Downloader.Fetch]
      fetchMsg.replyTo ! Downloader.Failed("http://example.com/test", "error message")
      
      val response = replyProbe.expectMessageType[Downloader.Failed]
      response.url should ===("http://example.com/test")
      response.reason should ===("error message")
    }
    
    "respect concurrency limit" in {
      val downloaderProbe = createTestProbe[Downloader.Command]("downloader")
      val queue = spawn(DownloadQueue(concurrencyLimit = 2, downloaderProbe.ref))
      
      val replies = (1 to 5).map { i =>
        val replyProbe = createTestProbe[Downloader.Response](s"reply-$i")
        queue ! DownloadQueue.Fetch(s"http://example.com/test$i", replyProbe.ref)
        replyProbe
      }
      
      // Only 2 requests should be sent to downloader (concurrencyLimit = 2)
      val fetch1 = downloaderProbe.expectMessageType[Downloader.Fetch]
      val fetch2 = downloaderProbe.expectMessageType[Downloader.Fetch]
      // The remaining 3 should not be sent yet
      downloaderProbe.expectNoMessage(100.millis)
      
      // Complete first request
      fetch1.replyTo ! Downloader.Downloaded(fetch1.url, "content1")
      
      // Now a 3rd request should be sent
      val fetch3 = downloaderProbe.expectMessageType[Downloader.Fetch]
      
      // Complete second request
      fetch2.replyTo ! Downloader.Downloaded(fetch2.url, "content2")
      
      // 4th request should be sent
      val fetch4 = downloaderProbe.expectMessageType[Downloader.Fetch]
      
      // Complete remaining requests
      fetch3.replyTo ! Downloader.Downloaded(fetch3.url, "content3")
      fetch4.replyTo ! Downloader.Downloaded(fetch4.url, "content4")
      
      // 5th request should be sent
      val fetch5 = downloaderProbe.expectMessageType[Downloader.Fetch]
      fetch5.replyTo ! Downloader.Downloaded(fetch5.url, "content5")
    }
    
    "process requests in FIFO order" in {
      val downloaderProbe = createTestProbe[Downloader.Command]("downloader")
      val queue = spawn(DownloadQueue(concurrencyLimit = 1, downloaderProbe.ref))
      
      val replies = (1 to 3).map { i =>
        val replyProbe = createTestProbe[Downloader.Response](s"reply-$i")
        queue ! DownloadQueue.Fetch(s"http://example.com/test$i", replyProbe.ref)
        replyProbe
      }
      
      // First request should be sent first
      val fetch1 = downloaderProbe.expectMessageType[Downloader.Fetch]
      fetch1.url should ===("http://example.com/test1")
      fetch1.replyTo ! Downloader.Downloaded("http://example.com/test1", "content1")
      replies(0).expectMessage(Downloader.Downloaded("http://example.com/test1", "content1"))
      
      // Second request
      val fetch2 = downloaderProbe.expectMessageType[Downloader.Fetch]
      fetch2.url should ===("http://example.com/test2")
      fetch2.replyTo ! Downloader.Downloaded("http://example.com/test2", "content2")
      replies(1).expectMessage(Downloader.Downloaded("http://example.com/test2", "content2"))
      
      // Third request
      val fetch3 = downloaderProbe.expectMessageType[Downloader.Fetch]
      fetch3.url should ===("http://example.com/test3")
    }
    
    "requeue on ask timeout without pausing" in {
      val downloaderProbe = createTestProbe[Downloader.Command]("downloader")
      val queue = spawn(DownloadQueue(concurrencyLimit = 1, downloaderProbe.ref, askTimeout = 50.millis))
      val replyProbe = createTestProbe[Downloader.Response]("reply")
      
      queue ! DownloadQueue.Fetch("http://example.com/test", replyProbe.ref)
      
      // Request is sent to downloader
      val fetch1 = downloaderProbe.expectMessageType[Downloader.Fetch]
      fetch1.url should ===("http://example.com/test")
      
      // Don't respond - let it timeout (askTimeout = 50ms)
      // After timeout, request should be re-queued and retried immediately (no pause)
      
      // Request should be retried quickly
      val fetch2 = downloaderProbe.expectMessageType[Downloader.Fetch](500.millis)
      fetch2.url should ===("http://example.com/test")
      
      // Now respond successfully
      fetch2.replyTo ! Downloader.Downloaded("http://example.com/test", "content")
      replyProbe.expectMessage(Downloader.Downloaded("http://example.com/test", "content"))
    }
    
    "send Failed on non-timeout exceptions" in {
      val downloaderProbe = createTestProbe[Downloader.Command]("downloader")
      val queue = spawn(DownloadQueue(concurrencyLimit = 2, downloaderProbe.ref))
      val replyProbe = createTestProbe[Downloader.Response]("reply")
      
      queue ! DownloadQueue.Fetch("http://example.com/test", replyProbe.ref)
      
      val fetchMsg = downloaderProbe.expectMessageType[Downloader.Fetch]
      // Simulate connection failure
      fetchMsg.replyTo ! Downloader.Failed("http://example.com/test", "connection refused")
      
      val response = replyProbe.expectMessageType[Downloader.Failed]
      response.url should ===("http://example.com/test")
      response.reason should ===("connection refused")
    }
    
    "pause after TooManyRequests using pauseDuration" in {
      val downloaderProbe = createTestProbe[Downloader.Command]("downloader")
      val queue = spawn(DownloadQueue(concurrencyLimit = 1, downloaderProbe.ref, pauseDuration = 500.millis))
      val replyProbe1 = createTestProbe[Downloader.Response]("reply1")
      val replyProbe2 = createTestProbe[Downloader.Response]("reply2")
      
      // Send first request
      queue ! DownloadQueue.Fetch("http://example.com/test1", replyProbe1.ref)
      val fetch1 = downloaderProbe.expectMessageType[Downloader.Fetch]
      
      // Send second request (should be queued)
      queue ! DownloadQueue.Fetch("http://example.com/test2", replyProbe2.ref)
      
      // First request returns 429
      fetch1.replyTo ! Downloader.TooManyRequests("http://example.com/test1")
      
      // Give time for 429 processing
      Thread.sleep(50)
      
      // Both requests should be queued, no responses yet
      replyProbe1.expectNoMessage(100.millis)
      replyProbe2.expectNoMessage(100.millis)
      
      // During pause (500ms), no new requests should be sent to downloader
      downloaderProbe.expectNoMessage(200.millis)
      
      // After pause expires, requests should resume
      // test1 was re-queued first, then test2 was already waiting
      // When test1 429'd, test1 goes to back of queue: queue is [test2, test1]
      // So test2 should be processed first after pause
      val retryFetch2 = downloaderProbe.expectMessageType[Downloader.Fetch](1.second)
      retryFetch2.url should ===("http://example.com/test2")
      retryFetch2.replyTo ! Downloader.Downloaded("http://example.com/test2", "content2")
      replyProbe2.expectMessage(Downloader.Downloaded("http://example.com/test2", "content2"))
      
      // Now test1 should be retried
      val retryFetch1 = downloaderProbe.expectMessageType[Downloader.Fetch]
      retryFetch1.url should ===("http://example.com/test1")
      retryFetch1.replyTo ! Downloader.Downloaded("http://example.com/test1", "content1")
      replyProbe1.expectMessage(Downloader.Downloaded("http://example.com/test1", "content1"))
    }
    
    "handle multiple concurrent requests" in {
      val downloaderProbe = createTestProbe[Downloader.Command]("downloader")
      val queue = spawn(DownloadQueue(concurrencyLimit = 3, downloaderProbe.ref))
      
      val replies = (1 to 3).map { i =>
        val replyProbe = createTestProbe[Downloader.Response](s"reply-$i")
        queue ! DownloadQueue.Fetch(s"http://example.com/test$i", replyProbe.ref)
        replyProbe
      }
      
      // All 3 should be sent immediately
      val fetchedUrls = (1 to 3).map { _ =>
        val fetch = downloaderProbe.expectMessageType[Downloader.Fetch]
        fetch.url
      }
      fetchedUrls.toSet should ===(Set("http://example.com/test1", "http://example.com/test2", "http://example.com/test3"))
    }
  }
}