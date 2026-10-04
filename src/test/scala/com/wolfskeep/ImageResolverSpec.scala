package com.wolfskeep

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import akka.actor.typed.ActorRef
import akka.actor.typed.scaladsl.Behaviors
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.BeforeAndAfterAll
import com.wolfskeep.rebrickable.LDrawImageFetcherTrait

import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class ImageResolverSpec extends ScalaTestWithActorTestKit with AnyWordSpecLike with BeforeAndAfterAll {

  import ImageResolver._

  private val readyBytes = Array[Byte](1, 2, 3)
  private val queryUrl = s"${BricksetPartFetcher.baseUrl}?query=3001"
  private val elementQueryUrl = s"${BricksetPartFetcher.baseUrl}?query=6331694"

  private val htmlWithImage =
    """<html><body><article class="set"><img src="https://example.com/3001.jpg" /></article></body></html>"""
  private val htmlWithoutImage =
    """<html><body><p>No article</p></body></html>"""

  private class StubLdraw(zipAvailable: Boolean, imageBytes: Option[Array[Byte]]) extends LDrawImageFetcherTrait {
    @volatile private var zipWarm: Boolean = zipAvailable
    @volatile private var warmBytes: Option[Array[Byte]] = imageBytes
    @volatile var allowDownload: Boolean = true
    val downloadLatch: CountDownLatch = new CountDownLatch(1)
    val downloadCount: AtomicInteger = new AtomicInteger(0)

    def ensureDownloaded(colorId: Int)(implicit ec: ExecutionContext): Boolean = {
      downloadCount.incrementAndGet()
      downloadLatch.await()
      zipWarm = true
      warmBytes = Some(readyBytes)
      true
    }
    def hasImageInZip(colorId: Int, partNumber: String): Boolean = warmBytes.isDefined
    def getImageFromZip(colorId: Int, partNumber: String): Option[Array[Byte]] = warmBytes
    def isZipAvailable(colorId: Int): Boolean = zipWarm
    def canRetryDownload(colorId: Int): Boolean = allowDownload

    def completeDownload(): Unit = downloadLatch.countDown()
  }

  private class CacheStub {
    @volatile private var store: Map[String, (String, Long)] = Map.empty
    val ref: ActorRef[DiskCache.Command] = spawn(Behaviors.receiveMessage[DiskCache.Command] {
      case DiskCache.Insert(key, value) =>
        store = store.updated(key, (value, System.currentTimeMillis()))
        Behaviors.same
      case DiskCache.Fetch(key, replyTo) =>
        store.get(key) match {
          case Some((value, insertedAt)) => replyTo ! DiskCache.FetchResult(key, value, insertedAt)
          case None                      => replyTo ! DiskCache.NotFound(key)
        }
        Behaviors.same
    })

    def put(key: String, value: String, insertedAt: Long): Unit =
      store = store.updated(key, (value, insertedAt))
    def get(key: String): Option[String] = store.get(key).map(_._1)
  }

  private class DownloaderStub {
    val fetchCount: AtomicInteger = new AtomicInteger(0)
    val responses: scala.collection.mutable.Map[String, CachedDownloader.Response] =
      scala.collection.mutable.Map.empty
    val delays: scala.collection.mutable.Map[String, CountDownLatch] =
      scala.collection.mutable.Map.empty
    val ref: ActorRef[CachedDownloader.Command] = spawn(Behaviors.receiveMessage[CachedDownloader.Command] {
      case CachedDownloader.Fetch(url, replyTo) =>
        fetchCount.incrementAndGet()
        delays.get(url).foreach(_.await())
        replyTo ! responses.getOrElse(
          url,
          CachedDownloader.Failed(url, new RuntimeException(s"no canned response for $url"))
        )
        Behaviors.same
    })

    def serve(url: String, response: CachedDownloader.Response): Unit = responses(url) = response
    def delay(url: String): CountDownLatch = {
      val latch = new CountDownLatch(1)
      delays(url) = latch
      latch
    }
  }

  private def until(cond: => Boolean, timeout: FiniteDuration = 5.seconds): Unit = {
    val deadline = timeout.fromNow
    while (!cond && deadline.hasTimeLeft()) Thread.sleep(20)
  }

  private def awaitLdrawAnswer(resolver: ActorRef[Command], colorId: Int, partNumber: String): LdrawImageResponse = {
    val probe = createTestProbe[LdrawImageResponse]()
    val deadline = 5.seconds.fromNow
    var answer: LdrawImageResponse = LdrawImagePending
    while (deadline.hasTimeLeft() && answer == LdrawImagePending) {
      resolver ! GetLdrawImage(colorId, partNumber, probe.ref)
      answer = probe.receiveMessage(1.second)
    }
    answer
  }

  private def awaitBricksetAnswer(resolver: ActorRef[Command], partNumber: String, elementId: Option[String] = None): BricksetImageResponse = {
    val probe = createTestProbe[BricksetImageResponse]()
    val deadline = 5.seconds.fromNow
    var answer: BricksetImageResponse = BricksetImagePending
    while (deadline.hasTimeLeft() && answer == BricksetImagePending) {
      resolver ! GetBricksetImageUrl(partNumber, elementId, probe.ref)
      answer = probe.receiveMessage(1.second)
    }
    answer
  }

  "ImageResolver" should {

    "answer Ready from a warm zip containing the image" in {
      val ldraw = new StubLdraw(zipAvailable = true, imageBytes = Some(readyBytes))
      val resolver = spawn(ImageResolver(ldraw, new DownloaderStub().ref, new CacheStub().ref))
      val probe = createTestProbe[LdrawImageResponse]()

      resolver ! GetLdrawImage(1, "3001", probe.ref)

      probe.expectMessage(LdrawImageReady(readyBytes))
    }

    "answer Unavailable from a warm zip without the image" in {
      val ldraw = new StubLdraw(zipAvailable = true, imageBytes = None)
      val resolver = spawn(ImageResolver(ldraw, new DownloaderStub().ref, new CacheStub().ref))
      val probe = createTestProbe[LdrawImageResponse]()

      resolver ! GetLdrawImage(1, "3001", probe.ref)

      probe.expectMessage(LdrawImageUnavailable)
    }

    "answer Pending and download once while concurrent queries share the flight" in {
      val ldraw = new StubLdraw(zipAvailable = false, imageBytes = None)
      val resolver = spawn(ImageResolver(ldraw, new DownloaderStub().ref, new CacheStub().ref))
      val probe = createTestProbe[LdrawImageResponse]()

      resolver ! GetLdrawImage(1, "3001", probe.ref)
      probe.expectMessage(LdrawImagePending)

      resolver ! GetLdrawImage(1, "3002", probe.ref)
      probe.expectMessage(LdrawImagePending)

      until(ldraw.downloadCount.get >= 1)
      ldraw.downloadCount.get should be(1)

      ldraw.completeDownload()
      awaitLdrawAnswer(resolver, 1, "3003") should matchPattern { case LdrawImageReady(_) => }
      ldraw.downloadCount.get should be(1)
    }

    "answer Unavailable when the zip download has exhausted retries" in {
      val ldraw = new StubLdraw(zipAvailable = false, imageBytes = None)
      ldraw.allowDownload = false
      val resolver = spawn(ImageResolver(ldraw, new DownloaderStub().ref, new CacheStub().ref))
      val probe = createTestProbe[LdrawImageResponse]()

      resolver ! GetLdrawImage(1, "3001", probe.ref)

      probe.expectMessage(LdrawImageUnavailable)
    }

    "mark Unavailable after a failed download once retries are exhausted" in {
      val ldraw = new StubLdraw(zipAvailable = false, imageBytes = None) {
        override def ensureDownloaded(colorId: Int)(implicit ec: ExecutionContext): Boolean = {
          downloadCount.incrementAndGet()
          false
        }
      }
      val resolver = spawn(ImageResolver(ldraw, new DownloaderStub().ref, new CacheStub().ref))
      val probe = createTestProbe[LdrawImageResponse]()

      resolver ! GetLdrawImage(1, "3001", probe.ref)
      probe.expectMessage(LdrawImagePending)

      ldraw.allowDownload = false
      awaitLdrawAnswer(resolver, 1, "3001") should be(LdrawImageUnavailable)
      ldraw.downloadCount.get should be(1)
    }

    "answer Resolved from the positive cache" in {
      val url = "https://example.com/3001.jpg"
      val cache = new CacheStub()
      cache.put(positiveKeyOf("3001"), url, System.currentTimeMillis())
      val resolver = spawn(ImageResolver(new StubLdraw(true, None), new DownloaderStub().ref, cache.ref))
      val probe = createTestProbe[BricksetImageResponse]()

      resolver ! GetBricksetImageUrl("3001", None, probe.ref)

      probe.expectMessage(BricksetImageResolved(url))
    }

    "answer Unavailable from the negative cache" in {
      val cache = new CacheStub()
      cache.put(negativeKeyOf("3001"), "1", System.currentTimeMillis())
      val resolver = spawn(ImageResolver(new StubLdraw(true, None), new DownloaderStub().ref, cache.ref))
      val probe = createTestProbe[BricksetImageResponse]()

      resolver ! GetBricksetImageUrl("3001", None, probe.ref)

      probe.expectMessage(BricksetImageUnavailable)
    }

    "re-resolve a part whose negative marker has expired" in {
      val cache = new CacheStub()
      cache.put(negativeKeyOf("3001"), "1", System.currentTimeMillis() - 25.hours.toMillis)
      val downloader = new DownloaderStub()
      downloader.serve(queryUrl, CachedDownloader.Downloaded(queryUrl, htmlWithImage))
      val resolver = spawn(
        ImageResolver(new StubLdraw(true, None), downloader.ref, cache.ref, negativeTtl = 24.hours))
      val probe = createTestProbe[BricksetImageResponse]()

      resolver ! GetBricksetImageUrl("3001", None, probe.ref)
      probe.expectMessage(BricksetImagePending)

      awaitBricksetAnswer(resolver, "3001") should be(
        BricksetImageResolved("https://example.com/3001.jpg")
      )
      cache.get(positiveKeyOf("3001")) should contain("https://example.com/3001.jpg")
      downloader.fetchCount.get should be(1)
    }

    "resolve a pending image once, persist it, and serve it after a resolver restart" in {
      val cache = new CacheStub()
      val downloader = new DownloaderStub()
      downloader.serve(queryUrl, CachedDownloader.Downloaded(queryUrl, htmlWithImage))
      val latch = downloader.delay(queryUrl)
      val resolver = spawn(ImageResolver(new StubLdraw(true, None), downloader.ref, cache.ref))
      val probe = createTestProbe[BricksetImageResponse]()

      resolver ! GetBricksetImageUrl("3001", None, probe.ref)
      probe.expectMessage(BricksetImagePending)

      resolver ! GetBricksetImageUrl("3001", None, probe.ref)
      probe.expectMessage(BricksetImagePending)

      latch.countDown()

      awaitBricksetAnswer(resolver, "3001") should be(BricksetImageResolved("https://example.com/3001.jpg"))
      downloader.fetchCount.get should be(1)
      cache.get(positiveKeyOf("3001")) should contain("https://example.com/3001.jpg")

      val restarted = spawn(ImageResolver(new StubLdraw(true, None), new DownloaderStub().ref, cache.ref))
      awaitBricksetAnswer(restarted, "3001") should be(BricksetImageResolved("https://example.com/3001.jpg"))
    }

    "try the element id query first and fall back to the part number" in {
      val cache = new CacheStub()
      val downloader = new DownloaderStub()
      downloader.serve(elementQueryUrl, CachedDownloader.Downloaded(elementQueryUrl, htmlWithoutImage))
      downloader.serve(queryUrl, CachedDownloader.Downloaded(queryUrl, htmlWithImage))
      val resolver = spawn(ImageResolver(new StubLdraw(true, None), downloader.ref, cache.ref))
      val probe = createTestProbe[BricksetImageResponse]()

      resolver ! GetBricksetImageUrl("3001", Some("6331694"), probe.ref)
      probe.expectMessage(BricksetImagePending)

      awaitBricksetAnswer(resolver, "3001", Some("6331694")) should be(
        BricksetImageResolved("https://example.com/3001.jpg")
      )
    }

    "persist a negative marker when nothing is found" in {
      val cache = new CacheStub()
      val downloader = new DownloaderStub()
      downloader.serve(queryUrl, CachedDownloader.Downloaded(queryUrl, htmlWithoutImage))
      val resolver = spawn(ImageResolver(new StubLdraw(true, None), downloader.ref, cache.ref))
      val probe = createTestProbe[BricksetImageResponse]()

      resolver ! GetBricksetImageUrl("3001", None, probe.ref)
      probe.expectMessage(BricksetImagePending)

      awaitBricksetAnswer(resolver, "3001") should be(BricksetImageUnavailable)
      cache.get(negativeKeyOf("3001")) should be(defined)
      downloader.fetchCount.get should be(1)
    }
  }

  private def positiveKeyOf(partNumber: String): String = s"brickset-image/$partNumber"
  private def negativeKeyOf(partNumber: String): String = s"brickset-image-missing/$partNumber"
}
