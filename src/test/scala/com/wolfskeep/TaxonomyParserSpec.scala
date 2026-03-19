package com.wolfskeep

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.io.Source

class TaxonomyParserSpec extends AnyFlatSpec with Matchers {
  "parseRootHtml" should "parse root.html correctly" in {
    val html = Source.fromFile("src/test/resources/root.html").mkString
    val categories = TaxonomyParser.parseRootHtml(html)

    categories should have size 12

    categories should contain (Category("1", "BASIC", None))
    categories should contain (Category("12", "TECHNIC", None))
  }

  "parseCategoryHtml" should "parse category-1.html correctly" in {
    val html = Source.fromFile("src/test/resources/category-1.html").mkString
    val url = "https://brickarchitect.com/parts/category-1?&retired=1&partstyle=1"
    val (categories, parts) = TaxonomyParser.parseCategoryHtml(url, html)

    // Check categories
    val basic = Category("1", "Basic", None)
    val brick = Category("15", "Brick", Some(basic))
    val oneByBrick = Category("27", "1× Brick", Some(brick))

    categories should contain (basic)
    categories should contain (brick)
    categories should contain (oneByBrick)

    // For parts
    val part1 = parts.find(_.partNumber == "3004")
    part1 shouldBe defined
    part1.get.name shouldBe "1×2 Brick"
    part1.get.categories.map(_.number) should contain allOf ("1", "15", "27")

    val part2 = parts.find(_.partNumber == "10202")
    part2 shouldBe defined
    part2.get.name shouldBe "6×6 Tile"
    part2.get.categories.map(_.number) should contain allOf ("1", "18", "38")
  }

  it should "populate sequence numbers in extraction order" in {
    val html = Source.fromFile("src/test/resources/category-1.html").mkString
    val url = "https://brickarchitect.com/parts/category-1"
    val (_, parts) = TaxonomyParser.parseCategoryHtml(url, html)

    val sortedBySeq = parts.sortBy(_.sequenceNumber)
    sortedBySeq.map(_.partNumber).distinct.length shouldBe sortedBySeq.length

    val firstPart = sortedBySeq.head
    firstPart.sequenceNumber should be > 0

    val lastPart = sortedBySeq.last
    lastPart.sequenceNumber should be > firstPart.sequenceNumber
  }

  "enhancePart" should "populate altNumbers from HTML" in {
    val html = Source.fromFile("src/test/resources/part-3069.html").mkString
    val part = LegoPart("3069", "1x2 Tile", Nil, 1, Set())
    val enhanced = TaxonomyParser.enhancePart(part, html)

    enhanced.altNumbers should contain ("3069a")
    enhanced.altNumbers should contain ("3069b")
    enhanced.altNumbers should contain ("30070")
    enhanced.altNumbers should contain ("37293")
    enhanced.altNumbers should not contain ("3069")
  }

  "parseCategoryHtml" should "extract image URLs for parts" in {
    val html = Source.fromFile("src/test/resources/category-1.html").mkString
    val url = "https://brickarchitect.com/parts/category-1"
    val (_, parts) = TaxonomyParser.parseCategoryHtml(url, html)

    parts.exists(_.imageUrl.isDefined) shouldBe true

    parts.foreach { part =>
      part.imageUrl match {
        case Some(imageUrl) =>
          imageUrl should endWith(s"${part.partNumber}.png")
        case None =>
          fail(s"Part ${part.partNumber} should have an image URL")
      }
      part.imageWidth shouldBe defined
      part.imageHeight shouldBe defined
    }
  }
}