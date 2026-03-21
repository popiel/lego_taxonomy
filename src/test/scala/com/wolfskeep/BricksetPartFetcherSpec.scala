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

  "matchBricklinkItemToTaxonomy" should "match exact item number" in {
    val taxonomyParts = List(
      LegoPart("30350bpb105", "Tile 2x3", List(Category("1", "Tile", None)), 1, Set.empty, None, None, None)
    )
    val result = BricksetPartFetcher.matchBricklinkItemToTaxonomy("30350bpb105", taxonomyParts)
    
    result shouldBe defined
    result.get.partNumber shouldBe "30350bpb105"
  }

  it should "match taxonomy part by stripping suffix and return full item number with modified name" in {
    val taxonomyParts = List(
      LegoPart("30350", "Tile 2x3", List(Category("1", "Tile", None)), 1, Set.empty, None, None, None)
    )
    val result = BricksetPartFetcher.matchBricklinkItemToTaxonomy("30350bpb105", taxonomyParts)
    
    result shouldBe defined
    result.get.partNumber shouldBe "30350bpb105"
    result.get.name shouldBe "Tile 2x3 (modified)"
    result.get.categories shouldBe taxonomyParts.head.categories
  }

  it should "return None when no match found" in {
    val taxonomyParts = List(
      LegoPart("99999", "Unknown Part", List(Category("1", "Unknown", None)), 1, Set.empty, None, None, None)
    )
    val result = BricksetPartFetcher.matchBricklinkItemToTaxonomy("30350bpb105", taxonomyParts)
    
    result shouldBe None
  }
}
