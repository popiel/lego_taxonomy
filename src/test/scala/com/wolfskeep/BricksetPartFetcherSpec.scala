package com.wolfskeep

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BricksetPartFetcherSpec extends AnyFlatSpec with Matchers {

  "findBricklinkUrl" should "find bricklink URL from Brickset HTML" in {
    val html = scala.io.Source.fromResource("brickset/69038.html").mkString
    val result = BricksetPartFetcher.findBricklinkUrl(html)
    
    result shouldBe defined
    result.get should include ("bricklink.com")
  }

  it should "return None for HTML without bricklink URL" in {
    val html = "<html><body><p>No bricklink here</p></body></html>"
    val result = BricksetPartFetcher.findBricklinkUrl(html)
    
    result shouldBe None
  }

  "parseBricklinkItem" should "extract item number from bricklink HTML" in {
    val html = scala.io.Source.fromResource("bricklink/30350bpb105.html").mkString
    val result = BricksetPartFetcher.parseBricklinkItem(html)
    
    result shouldBe defined
    result.get shouldBe "30350bpb105"
  }

  it should "return None for HTML without item number" in {
    val html = "<html><body><h1 id=\"item-name-title\">Some Part</h1></body></html>"
    val result = BricksetPartFetcher.parseBricklinkItem(html)
    
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

  it should "match stripped item number" in {
    val taxonomyParts = List(
      LegoPart("30350", "Tile 2x3", List(Category("1", "Tile", None)), 1, Set.empty, None, None, None)
    )
    val result = BricksetPartFetcher.matchBricklinkItemToTaxonomy("30350bpb105", taxonomyParts)
    
    result shouldBe defined
    result.get.partNumber shouldBe "30350"
    result.get.categories.map(_.name) should contain ("Tile")
  }

  it should "return None when no match found" in {
    val taxonomyParts = List(
      LegoPart("99999", "Unknown Part", List(Category("1", "Unknown", None)), 1, Set.empty, None, None, None)
    )
    val result = BricksetPartFetcher.matchBricklinkItemToTaxonomy("30350bpb105", taxonomyParts)
    
    result shouldBe None
  }
}
