package com.wolfskeep

import akka.actor.typed.ActorRef
import akka.actor.typed.scaladsl.AskPattern._
import akka.util.Timeout
import akka.actor.typed.Scheduler
import org.slf4j.{Logger, LoggerFactory}
import scala.concurrent.Future
import scala.concurrent.duration._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import scala.collection.JavaConverters._

object BricksetPartFetcher {
  val baseUrl = "https://brickset.com/parts"
  private val log = LoggerFactory.getLogger(classOf[BricksetPartFetcher.type])

  case class BricksetPartResult(
    partNumber: String,
    elementId: Option[String],
    imageUrl: Option[String]
  )

  def fetchPartDetails(
    downloader: ActorRef[CachedDownloader.Command],
    partNumber: String,
    elementId: Option[String]
  )(implicit timeout: Timeout, scheduler: Scheduler, ec: scala.concurrent.ExecutionContext): Future[Option[BricksetPartResult]] = {
    val url = s"$baseUrl?query=$partNumber"
    downloader.ask(CachedDownloader.Fetch(url, _)).flatMap {
      case CachedDownloader.Downloaded(_, content) =>
        parsePartDetails(partNumber, elementId, content)
      case CachedDownloader.Failed(_, reason) =>
        Future.successful(None)
    }
  }

  def parsePartDetails(
    queryPartNumber: String,
    elementIdFromCsv: Option[String],
    html: String
  ): Future[Option[BricksetPartResult]] = Future.successful {
    val doc = Jsoup.parse(html)
    val article = doc.selectFirst("article.set")
    if (article == null) {
      return Future.successful(None)
    }

    val img = article.selectFirst("img")
    val imageUrl = if (img != null) {
      Option(img.attr("src")).filter(_.nonEmpty)
    } else None

    val elementId = elementIdFromCsv.orElse(extractElementNumberFromHtml(html))

    if (imageUrl.isEmpty && elementId.isEmpty) {
      None
    } else {
      Some(BricksetPartResult(
        partNumber = queryPartNumber,
        elementId = elementId,
        imageUrl = imageUrl
      ))
    }
  }

  private def extractElementNumberFromHtml(html: String): Option[String] = {
    val doc = Jsoup.parse(html)
    val tagsDiv = doc.selectFirst("div.tags")
    Option(tagsDiv).flatMap { div =>
      div.select("a").asScala.find { a =>
        val href = a.attr("href")
        href.startsWith("/parts/") &&
        !href.contains("/design-") &&
        !href.contains("/platform-") &&
        !href.contains("/category-") &&
        !href.contains("/year-") &&
        !href.contains("/colour-")
      }.filter(a => a.text.forall(_.isDigit))
      .map(_.text)
    }
  }

  def matchBricklinkItemToTaxonomy(itemNumber: String, taxonomyParts: List[LegoPart]): Option[LegoPart] = {
    taxonomyParts.find(_.partNumber == itemNumber).orElse {
      val strippedNumber = stripTrailingLetters(itemNumber)
      if (strippedNumber != itemNumber) {
        taxonomyParts.find(_.partNumber == strippedNumber)
      } else {
        None
      }
    }
  }

  private def stripTrailingLetters(s: String): String = {
    val regex = """^(\d+)[a-zA-Z].*""".r
    s match {
      case regex(digits) => digits
      case _ => s
    }
  }
}
