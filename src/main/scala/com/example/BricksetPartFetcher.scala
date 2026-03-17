package com.example

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

  def fetchPartDetails(downloader: ActorRef[CachedDownloader.Command], partNumber: String)(implicit timeout: Timeout, scheduler: Scheduler, ec: scala.concurrent.ExecutionContext): Future[Option[LegoPart]] = {
    val url = s"$baseUrl?query=$partNumber"
    downloader.ask(CachedDownloader.Fetch(url, _)).map {
      case CachedDownloader.Downloaded(_, content) =>
        parsePartDetails(partNumber, content)
      case CachedDownloader.Failed(_, reason) =>
        None
    }
  }

  def parsePartDetails(queryPartNumber: String, html: String): Option[LegoPart] = {
    val doc = Jsoup.parse(html)
    val article = doc.selectFirst("article.set")
    if (article == null) {
      return None
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
      None
    } else {
      Some(LegoPart(
        partNumber = queryPartNumber,
        name = name,
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
