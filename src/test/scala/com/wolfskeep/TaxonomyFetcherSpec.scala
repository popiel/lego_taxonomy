package com.wolfskeep

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import org.scalatest.wordspec.AnyWordSpecLike

import scala.io.Source
import scala.concurrent.duration._

import akka.actor.typed.scaladsl.Behaviors
import com.wolfskeep.rebrickable.RebrickableHolder

class TaxonomyFetcherSpec extends ScalaTestWithActorTestKit with AnyWordSpecLike {

  private val rootHtml = Source.fromFile("src/test/resources/root.html").mkString
  private val category1Html = Source.fromFile("src/test/resources/category-1.html").mkString
  private val category2Html = Source.fromFile("src/test/resources/category-2.html").mkString

  // minimal valid pages: parseCategoryHtml needs div.main h1 + div.inlineresults;
  // enhancePart needs no span.part_num (empty altNumbers).
  private val emptyCategoryHtml = """<html><body><div class="main"><h1>Empty</h1></div><div class="inlineresults"></div></body></html>"""
  private val emptyPartHtml = """<html><body></body></html>"""
  private val part3004Html = """<html><body><span class="part_num">3004</span><span class="part_num">3004a</span><span class="part_num">9999</span></body></html>"""

  private val category1Url = "https://brickarchitect.com/parts/category-1?&retired=1&partstyle=1"
  private val category2Url = "https://brickarchitect.com/parts/category-2?&retired=1&partstyle=1"

  private val (category1Parts, category2Parts) = {
    val (_, cat1Parts) = TaxonomyParser.parseCategoryHtml(category1Url, category1Html)
    val (_, cat2Parts) = TaxonomyParser.parseCategoryHtml(category2Url, category2Html)
    (cat1Parts, cat2Parts)
  }
  private val totalParts = category1Parts.size + category2Parts.size

  private def partNumberOf(url: String): String =
    url.stripPrefix("https://brickarchitect.com/parts/").takeWhile(_ != '?')

  private class Fixture {
    val cacheProbe = createTestProbe[DiskCache.Command]()
    val downloaderProbe = createTestProbe[CachedDownloader.Command]()
    val holderProbe = createTestProbe[TaxonomyHolder.Command]()
    val askerProbe = createTestProbe[TaxonomyFetcher.Response]()
    val fetcher = spawn(TaxonomyFetcher(downloaderProbe.ref, cacheProbe.ref), s"fetcher-${scala.util.Random.nextInt(100000)}")

    def start(): Unit = {
      fetcher ! TaxonomyFetcher.RegisterHolder(holderProbe.ref)
      fetcher ! TaxonomyFetcher.GetTaxonomy(askerProbe.ref)
    }

    def serveRoot(): Unit = {
      val fetch = downloaderProbe.expectMessageType[CachedDownloader.Fetch]
      fetch.url shouldBe TaxonomyFetcher.rootUrl
      fetch.replyTo ! CachedDownloader.Downloaded(fetch.url, rootHtml)
    }

    def serveCategories(failFirst: Boolean = false): Unit = {
      (1 to 12).foreach { i =>
        val fetch = downloaderProbe.expectMessageType[CachedDownloader.Fetch](3.seconds)
        fetch.url should startWith("https://brickarchitect.com/parts/category-")
        if (failFirst && i == 1) {
          fetch.replyTo ! CachedDownloader.Failed(fetch.url, new RuntimeException("test category failure"))
        } else {
          val content = fetch.url match {
            case u if u.contains("category-1?") => category1Html
            case u if u.contains("category-2?") => category2Html
            case _ => emptyCategoryHtml
          }
          fetch.replyTo ! CachedDownloader.Downloaded(fetch.url, content)
        }
      }
    }

    def servePartPages(failPart: Option[String] = None): Unit = {
      (1 to totalParts).foreach { _ =>
        val fetch = downloaderProbe.expectMessageType[CachedDownloader.Fetch](3.seconds)
        fetch.url should startWith("https://brickarchitect.com/parts/")
        failPart match {
          case Some(partNumber) if partNumberOf(fetch.url) == partNumber =>
            fetch.replyTo ! CachedDownloader.Failed(fetch.url, new RuntimeException("test part failure"))
          case _ =>
            val content = partNumberOf(fetch.url) match {
              case "3004" => part3004Html
              case _ => emptyPartHtml
            }
            fetch.replyTo ! CachedDownloader.Downloaded(fetch.url, content)
        }
      }
    }

    def collectAugmentations(expected: Int): Seq[(String, Set[String])] =
      (1 to expected).map { _ =>
        val msg = holderProbe.expectMessageType[TaxonomyHolder.AugmentPart](3.seconds)
        (msg.partNumber, msg.altNumbers)
      }
  }

  "TaxonomyFetcher" should {

    "publish the bulk taxonomy to the holder before augmentation starts" in new Fixture {
      start()
      serveRoot()
      serveCategories()

      val setMsg = holderProbe.expectMessageType[TaxonomyHolder.SetTaxonomy](3.seconds)
      setMsg.taxonomyData.parts.map(_.partNumber) should contain("3004")
      setMsg.taxonomyData.parts.exists(_.altNumbers.nonEmpty) shouldBe false
      setMsg.taxonomyData.categories should not be empty
    }

    "send AugmentPart for each fetched part page and complete" in new Fixture {
      start()
      serveRoot()
      serveCategories()
      holderProbe.expectMessageType[TaxonomyHolder.SetTaxonomy](3.seconds)
      servePartPages()

      val augmentations = collectAugmentations(totalParts)
      augmentations should contain(("3004", Set("3004a", "9999")))
      augmentations.count { case (num, _) => num == "3004" } shouldBe 1
      (augmentations.filter(_._1 != "3004").map(_._2)).forall(_.isEmpty) shouldBe true

      askerProbe.expectMessage(TaxonomyFetcher.AugmentationComplete)
      askerProbe.expectNoMessage(200.millis)
    }

    "skip a failed part page, keep the taxonomy intact, and still complete" in new Fixture {
      val deafRebrickable = spawn(Behaviors.ignore[RebrickableHolder.Command])
      val holder = spawn(TaxonomyHolder(deafRebrickable))
      fetcher ! TaxonomyFetcher.RegisterHolder(holder)
      fetcher ! TaxonomyFetcher.GetTaxonomy(askerProbe.ref)

      serveRoot()
      serveCategories()
      // holder is a real actor; bulk publish happens without any probe message to check here
      servePartPages(failPart = Some("3004"))

      askerProbe.expectMessage(TaxonomyFetcher.AugmentationComplete)
      askerProbe.expectNoMessage(200.millis)

      val readProbe = createTestProbe[TaxonomyHolder.Response]()
      holder ! TaxonomyHolder.GetTaxonomy(readProbe.ref)
      val data = readProbe.expectMessageType[TaxonomyHolder.TaxonomyDataResponse].taxonomyData

      data.parts.map(_.partNumber) should contain("3004")
      data.findPart("3004").map(_.altNumbers) shouldBe Some(Set.empty)
      data.parts.size shouldBe totalParts
    }

    "reply Failed and never touch the holder when a category fetch fails" in new Fixture {
      start()
      serveRoot()
      serveCategories(failFirst = true)

      askerProbe.expectMessageType[TaxonomyFetcher.Failed]
      holderProbe.expectNoMessage(300.millis)
    }

    "complete without a registered holder" in new Fixture {
      fetcher ! TaxonomyFetcher.GetTaxonomy(askerProbe.ref)
      serveRoot()
      serveCategories()
      servePartPages()

      askerProbe.expectMessage(TaxonomyFetcher.AugmentationComplete)
    }
  }
}
