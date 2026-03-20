package com.wolfskeep

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BricksetPartFetcherSpec extends AnyFlatSpec with Matchers {

  "findElementNumber" should "extract element number from Brickset HTML" in {
    val html = scala.io.Source.fromResource("brickset/69038.html").mkString
    val result = BricksetPartFetcher.findElementNumber(html)
    
    result shouldBe Some("6331694")
  }

  it should "return None for HTML without valid element number" in {
    val html = "<html><body><div class='tags'><a href='/parts/design-123'>Design</a></div></body></html>"
    val result = BricksetPartFetcher.findElementNumber(html)
    
    result shouldBe None
  }

  it should "return None when text is not numeric" in {
    val html = "<html><body><div class='tags'><a href='/parts/123abc'>123abc</a></div></body></html>"
    val result = BricksetPartFetcher.findElementNumber(html)
    
    result shouldBe None
  }

  it should "return None for HTML without div.tags" in {
    val html = "<html><body><p>No tags here</p></body></html>"
    val result = BricksetPartFetcher.findElementNumber(html)
    
    result shouldBe None
  }

  it should "skip links with category prefix" in {
    val html = "<html><body><div class='tags'><a href='/parts/category-Plates'>Plates</a></div></body></html>"
    val result = BricksetPartFetcher.findElementNumber(html)
    
    result shouldBe None
  }

  it should "skip links with platform prefix" in {
    val html = "<html><body><div class='tags'><a href='/parts/platform-System'>System</a></div></body></html>"
    val result = BricksetPartFetcher.findElementNumber(html)
    
    result shouldBe None
  }

  it should "skip links with year prefix" in {
    val html = "<html><body><div class='tags'><a href='/parts/year-2020'>2020</a></div></body></html>"
    val result = BricksetPartFetcher.findElementNumber(html)
    
    result shouldBe None
  }

  it should "skip links with colour prefix" in {
    val html = "<html><body><div class='tags'><a href='/parts/colour-Red'>Red</a></div></body></html>"
    val result = BricksetPartFetcher.findElementNumber(html)
    
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
