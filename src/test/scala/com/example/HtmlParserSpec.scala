package com.example

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.io.Source

class HtmlParserSpec extends AnyFlatSpec with Matchers {

  "parseHtml" should "parse category-1.html correctly" in {
    val html = Source.fromFile("src/test/resources/category-1.html").mkString
    val url = "https://brickarchitect.com/parts/category-1?&retired=1&partstyle=1"
    val (categories, parts) = HtmlParser.parseHtml(html, url)

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
}