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

  def fetchPartDetails(
    downloader: ActorRef[CachedDownloader.Command],
    partNumber: String,
    elementId: Option[String],
    taxonomyParts: List[LegoPart],
    bricklinkActor: ActorRef[BricklinkActor.Command]
  )(implicit timeout: Timeout, scheduler: Scheduler, ec: scala.concurrent.ExecutionContext): Future[Option[LegoPart]] = {
    val url = s"$baseUrl?query=$partNumber"
    downloader.ask(CachedDownloader.Fetch(url, _)).flatMap {
      case CachedDownloader.Downloaded(_, content) =>
        parsePartDetails(partNumber, elementId, content, taxonomyParts, downloader, taxonomyParts, bricklinkActor)
      case CachedDownloader.Failed(_, reason) =>
        Future.successful(None)
    }
  }

  def parsePartDetails(
    queryPartNumber: String,
    elementIdFromCsv: Option[String],
    html: String,
    taxonomyParts: List[LegoPart],
    downloader: ActorRef[CachedDownloader.Command],
    taxonomy: List[LegoPart],
    bricklinkActor: ActorRef[BricklinkActor.Command]
  )(implicit timeout: Timeout, scheduler: Scheduler, ec: scala.concurrent.ExecutionContext): Future[Option[LegoPart]] = {
    val doc = Jsoup.parse(html)
    val article = doc.selectFirst("article.set")
    if (article == null) {
      return Future.successful(None)
    }

    val img = article.selectFirst("img")
    val imageUrl = if (img != null) {
      Option(img.attr("src")).filter(_.nonEmpty)
    } else None

    val metaDiv = article.selectFirst("div.meta")
    val h1: Option[Element] = if (metaDiv != null) Option(metaDiv.selectFirst("h1")) else None
    val name = h1 match {
      case Some(element) =>
        element.text().trim match {
          case text if text.contains(": ") =>
            text.substring(text.indexOf(": ") + 2).trim
          case text => text.trim
        }
      case None => ""
    }

    if (name.isEmpty && imageUrl.isEmpty) {
      return Future.successful(None)
    }

    val resolvedElementId = elementIdFromCsv.orElse(findElementNumber(html))
    
    resolvedElementId match {
      case Some(elemNum) =>
        log.info(s"Using element number $elemNum for Brickset part $queryPartNumber")
        val bricklinkTimeout: Timeout = 15.seconds
        bricklinkActor.ask(BricklinkActor.GetItemNumberByElementId(elemNum, _))(bricklinkTimeout, scheduler).map {
          case BricklinkActor.ItemMappingResponse(itemNo, itemType) =>
            log.info(s"BrickLink API returned itemNo=$itemNo, type=$itemType for element $elemNum")
            val matchedPart = matchBricklinkItemToTaxonomy(itemNo, taxonomy)
            matchedPart.map { tp =>
              LegoPart(
                partNumber = tp.partNumber,
                name = tp.name,
                categories = tp.categories,
                sequenceNumber = tp.sequenceNumber,
                altNumbers = tp.altNumbers,
                imageUrl = imageUrl,
                imageWidth = tp.imageWidth,
                imageHeight = tp.imageHeight
              )
            }.orElse {
              Some(LegoPart(
                partNumber = "",
                name = "",
                categories = Nil,
                sequenceNumber = 0,
                altNumbers = Set.empty,
                imageUrl = imageUrl,
                imageWidth = None,
                imageHeight = None
              ))
            }
          case BricklinkActor.Failed(message) =>
            log.warn(s"BrickLink API failed for element $elemNum: $message")
            Some(LegoPart(
              partNumber = "",
              name = "",
              categories = Nil,
              sequenceNumber = 0,
              altNumbers = Set.empty,
              imageUrl = imageUrl,
              imageWidth = None,
              imageHeight = None
            ))
        }
      case None =>
        Future.successful {
          Some(LegoPart(
            partNumber = "",
            name = "",
            categories = Nil,
            sequenceNumber = 0,
            altNumbers = Set.empty,
            imageUrl = imageUrl,
            imageWidth = None,
            imageHeight = None
          ))
        }
    }
  }

  def findElementNumber(html: String): Option[String] = {
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
