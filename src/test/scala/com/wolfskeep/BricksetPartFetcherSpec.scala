package com.wolfskeep

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.concurrent.Await
import scala.concurrent.duration._

class BricksetPartFetcherSpec extends AnyFlatSpec with Matchers {

  "BricksetPartFetcher" should "parse part details and extract element number from HTML" in {
    val html = scala.io.Source.fromResource("brickset/69038.html").mkString
    val result = Await.result(
      BricksetPartFetcher.parsePartDetails("69038", None, html),
      1.seconds
    )
    
    result shouldBe defined
    result.get.partNumber shouldBe "69038"
    result.get.elementId shouldBe Some("6331694")
    result.get.imageUrl shouldBe defined
    result.get.imageUrl.get should include ("6331694.jpg")
  }

  it should "prefer elementId from CSV over HTML extraction" in {
    val html = scala.io.Source.fromResource("brickset/69038.html").mkString
    val result = Await.result(
      BricksetPartFetcher.parsePartDetails("69038", Some("123456"), html),
      1.seconds
    )
    
    result shouldBe defined
    result.get.elementId shouldBe Some("123456")
  }

  it should "return None for HTML without article" in {
    val html = "<html><body><p>No article</p></body></html>"
    val result = Await.result(
      BricksetPartFetcher.parsePartDetails("12345", None, html),
      1.seconds
    )
    
    result shouldBe None
  }

  it should "return None for HTML without element number or image" in {
    val html = """<html><body><article class="set"></article></body></html>"""
    val result = Await.result(
      BricksetPartFetcher.parsePartDetails("12345", None, html),
      1.seconds
    )
    
    result shouldBe None
  }

  "findBasePart" should "match exact item number" in {
    val taxonomyParts = List(
      LegoPart("30350bpb105", "Tile 2x3", List(Category("1", "Tile", None)), 1, Set.empty, None, None, None)
    )
    val taxonomyData = TaxonomyData(Set.empty, taxonomyParts)
    val result = taxonomyData.findBasePart("30350bpb105")
    
    result shouldBe defined
    result.get.partNumber shouldBe "30350bpb105"
  }
  
  it should "match taxonomy part by stripping suffix and return full item number with modified name" in {
    val taxonomyParts = List(
      LegoPart("30350", "Tile 2x3", List(Category("1", "Tile", None)), 1, Set.empty, None, None, None)
    )
    val taxonomyData = TaxonomyData(Set.empty, taxonomyParts)
    val result = taxonomyData.findBasePart("30350bpb105")
    
    result shouldBe defined
    result.get.partNumber shouldBe "30350bpb105"
    result.get.name shouldBe "Tile 2x3 (modified)"
    result.get.categories shouldBe taxonomyParts.head.categories
  }
  
  it should "synthesize a modified identity without the base part's image" in {
    val curve = Category("1", "Curve", None)
    val round = Category("2", "Round", Some(curve))
    val tile = Category("3", "Tile", Some(round))
    val taxonomyParts = List(
      LegoPart(
        "98138", "Tile Round 1 x 1", List(curve, round, tile), 1, Set.empty,
        Some("https://brickarchitect.com/label/partqrcode.php?part_num=98138"), Some("100"), Some("60")
      )
    )
    val taxonomyData = TaxonomyData(Set.empty, taxonomyParts)
    val result = taxonomyData.findBasePart("98138pr0035")

    result shouldBe defined
    result.get.partNumber shouldBe "98138pr0035"
    result.get.name shouldBe "Tile Round 1 x 1 (modified)"
    result.get.categories shouldBe List(curve, round, tile)
    result.get.imageUrl shouldBe None
    result.get.imageWidth shouldBe None
    result.get.imageHeight shouldBe None
  }
  
  it should "return None when no match found" in {
    val taxonomyParts = List(
      LegoPart("99999", "Unknown Part", List(Category("1", "Unknown", None)), 1, Set.empty, None, None, None)
    )
    val taxonomyData = TaxonomyData(Set.empty, taxonomyParts)
    val result = taxonomyData.findBasePart("30350bpb105")
    
    result shouldBe None
  }
}
