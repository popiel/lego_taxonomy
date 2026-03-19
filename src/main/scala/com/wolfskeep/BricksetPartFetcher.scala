package com.wolfskeep

import akka.actor.typed.ActorRef
import akka.actor.typed.scaladsl.AskPattern._
import akka.util.Timeout
import akka.actor.typed.Scheduler
import scala.concurrent.Future
import scala.concurrent.duration._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import scala.collection.JavaConverters._

object BricksetPartFetcher {
  val baseUrl = "https://brickset.com/parts"
  val bricklinkBaseUrl = "https://www.bricklink.com"

  def fetchPartDetails(downloader: ActorRef[CachedDownloader.Command], partNumber: String, taxonomyParts: List[LegoPart])(implicit timeout: Timeout, scheduler: Scheduler, ec: scala.concurrent.ExecutionContext): Future[Option[LegoPart]] = {
    val url = s"$baseUrl?query=$partNumber"
    downloader.ask(CachedDownloader.Fetch(url, _)).flatMap {
      case CachedDownloader.Downloaded(_, content) =>
        parsePartDetails(partNumber, content, taxonomyParts, downloader, taxonomyParts)
      case CachedDownloader.Failed(_, reason) =>
        Future.successful(None)
    }
  }

  def parsePartDetails(queryPartNumber: String, html: String, taxonomyParts: List[LegoPart], downloader: ActorRef[CachedDownloader.Command], taxonomy: List[LegoPart])(implicit timeout: Timeout, scheduler: Scheduler, ec: scala.concurrent.ExecutionContext): Future[Option[LegoPart]] = {
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

    val bricklinkUrl = findBricklinkUrl(html)
    
    bricklinkUrl match {
      case Some(url) =>
        val fetchUrl = if (url.startsWith("http")) url else s"$bricklinkBaseUrl$url"
        downloader.ask(CachedDownloader.Fetch(fetchUrl, _)).map {
          case CachedDownloader.Downloaded(_, bricklinkHtml) =>
            val itemNumber = parseBricklinkItem(bricklinkHtml)
            val matchedPart = itemNumber.flatMap(matchBricklinkItemToTaxonomy(_, taxonomy))
            matchedPart.orElse {
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
          case CachedDownloader.Failed(_, _) =>
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

  def findBricklinkUrl(html: String): Option[String] = {
    val doc = Jsoup.parse(html)
    
    val valueNewDl = doc.select("dl").asScala.find { dl =>
      dl.text().contains("Value new")
    }
    
    valueNewDl.flatMap { dl =>
      val bricklinkLink = dl.select("a").asScala.find { a =>
        a.attr("href").contains("bricklink.com")
      }
      bricklinkLink.map(_.attr("href"))
    }
  }

  def parseBricklinkItem(html: String): Option[String] = {
    val doc = Jsoup.parse(html)
    
    val h1 = doc.selectFirst("h1#item-name-title")
    
    if (h1 == null) return None
    
    val parent = h1.parent()
    if (parent == null) return None
    
    var sibling = parent.nextElementSibling()
    
    while (sibling != null && sibling.tagName() != "span") {
      sibling = sibling.nextElementSibling()
    }
    
    if (sibling == null) return None
    
    val spanWithItemNo = sibling.text()
    if (!spanWithItemNo.contains("Item No:")) return None
    
    val innerSpan = sibling.children().first()
    val itemNumber = if (innerSpan != null) {
      innerSpan.text().trim
    } else {
      return None
    }
    
    if (itemNumber.nonEmpty) Some(itemNumber) else None
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
