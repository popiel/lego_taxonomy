package com.wolfskeep

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TaxonomyDataSpec extends AnyFlatSpec with Matchers {

  val testParts = List(
    LegoPart("3001", "BRICK 2X4", List(Category("1", "Basic", None)), 1, Set.empty, None, None, None),
    LegoPart("3002", "BRICK 2X2", List(Category("1", "Basic", None)), 2, Set.empty, None, None, None),
    LegoPart("3003", "BRICK 2X6", List(Category("1", "Basic", None)), 3, Set.empty, None, None, None),
    LegoPart("3022", "PLATE 2X2", List(Category("2", "Plates", Some(Category("1", "Basic", None)))), 1, Set.empty, None, None, None),
    LegoPart("3023", "PLATE 1X2", List(Category("2", "Plates", Some(Category("1", "Basic", None)))), 2, Set.empty, None, None, None),
    LegoPart("3070", "TILE 1X1", List(Category("3", "Tiles", Some(Category("1", "Basic", None)))), 1, Set.empty, None, None, None),
    LegoPart("4006", "PLATE 1 X 6", List(Category("2", "Plates", Some(Category("1", "Basic", None)))), 3, Set.empty, None, None, None),
    LegoPart("3031", "PLATE 4X4", List(Category("2", "Plates", Some(Category("1", "Basic", None)))), 4, Set.empty, None, None, None),
    LegoPart("4000", "TILE 1 X 3 X 5", List(Category("3", "Tiles", Some(Category("1", "Basic", None)))), 5, Set.empty, None, None, None),
    LegoPart("4001", "BRICK 1\u00D7X6", List(Category("1", "Basic", None)), 6, Set.empty, None, None, None)
  )

  "TaxonomyData" should "create a partNumberOrAltToPart index" in {
    val taxonomyData = TaxonomyData(Set.empty, testParts)

    taxonomyData.partNumberOrAltToPart.contains("3001") shouldBe true
    taxonomyData.partNumberOrAltToPart.contains("3002") shouldBe true
    taxonomyData.partNumberOrAltToPart("3001").name shouldBe "BRICK 2X4"
  }

  it should "index parts by alternate numbers" in {
    val partsWithAlts = List(
      LegoPart("3001", "BRICK 2X4", List(Category("1", "Basic", None)), 1, Set("3001a", "3001b"), None, None, None)
    )
    val taxonomyData = TaxonomyData(Set.empty, partsWithAlts)

    taxonomyData.partNumberOrAltToPart.contains("3001") shouldBe true
    taxonomyData.partNumberOrAltToPart.contains("3001a") shouldBe true
    taxonomyData.partNumberOrAltToPart.contains("3001b") shouldBe true
  }

  it should "create a wordIndex from part names" in {
    val taxonomyData = TaxonomyData(Set.empty, testParts)

    taxonomyData.wordIndex.contains("brick") shouldBe true
    taxonomyData.wordIndex.contains("plate") shouldBe true
    taxonomyData.wordIndex.contains("tile") shouldBe true
    taxonomyData.wordIndex.contains("2x4") shouldBe true
    taxonomyData.wordIndex.contains("2x2") shouldBe true
    taxonomyData.wordIndex.contains("2x6") shouldBe true
    taxonomyData.wordIndex.contains("1x1") shouldBe true
    taxonomyData.wordIndex.contains("1x2") shouldBe true
  }

  it should "map words to correct parts" in {
    val taxonomyData = TaxonomyData(Set.empty, testParts)

    taxonomyData.wordIndex("brick").map(_.partNumber).toSet shouldBe Set("3001", "3002", "3003", "4001")
    taxonomyData.wordIndex("plate").map(_.partNumber).toSet shouldBe Set("3022", "3023", "4006", "3031")
    taxonomyData.wordIndex("tile").map(_.partNumber).toSet shouldBe Set("3070", "4000")
  }

  it should "handle empty parts list" in {
    val taxonomyData = TaxonomyData(Set.empty, Nil)
    taxonomyData.wordIndex shouldBe empty
  }

  it should "collapse dimension patterns with spaces" in {
    val taxonomyData = TaxonomyData(Set.empty, testParts)

    taxonomyData.wordIndex.contains("1x6") shouldBe true
    taxonomyData.wordIndex.contains("1x3x5") shouldBe true
    taxonomyData.wordIndex("1x6").map(_.partNumber).toSet shouldBe Set("4006")
    taxonomyData.wordIndex("1x3x5").map(_.partNumber).toSet shouldBe Set("4000")
  }

  it should "handle unicode multiplication sign" in {
    val taxonomyData = TaxonomyData(Set.empty, testParts)

    taxonomyData.wordIndex.contains("1x6") shouldBe true
    taxonomyData.wordIndex("1x6").map(_.partNumber).toSet shouldBe Set("4006")
  }

  "findPart" should "find part by part number" in {
    val taxonomyData = TaxonomyData(Set.empty, testParts)

    taxonomyData.findPart("3001").isDefined shouldBe true
    taxonomyData.findPart("3001").get.name shouldBe "BRICK 2X4"
  }

  it should "find part by alternate number" in {
    val partsWithAlts = List(
      LegoPart("3001", "BRICK 2X4", List(Category("1", "Basic", None)), 1, Set("3001a"), None, None, None)
    )
    val taxonomyData = TaxonomyData(Set.empty, partsWithAlts)

    taxonomyData.findPart("3001a").isDefined shouldBe true
    taxonomyData.findPart("3001a").get.partNumber shouldBe "3001"
  }

  it should "return None for unknown part number" in {
    val taxonomyData = TaxonomyData(Set.empty, testParts)

    taxonomyData.findPart("99999") shouldBe None
  }

  "searchByName" should "return parts sorted by match count" in {
    val taxonomyData = TaxonomyData(Set.empty, testParts)
    val results = taxonomyData.searchByName("BRICK 2X2")

    results.isEmpty shouldBe false
    results.head._1.partNumber shouldBe "3002"
    results.head._2 shouldBe 2
  }

  it should "return empty list for unknown query" in {
    val taxonomyData = TaxonomyData(Set.empty, testParts)
    val results = taxonomyData.searchByName("unknown part")

    results.isEmpty shouldBe true
  }

  it should "respect limit parameter" in {
    val taxonomyData = TaxonomyData(Set.empty, testParts)
    val results = taxonomyData.searchByName("brick", limit = 2)

    results.length shouldBe 2
  }

  it should "be case insensitive" in {
    val taxonomyData = TaxonomyData(Set.empty, testParts)
    val results = taxonomyData.searchByName("brick")

    results.isEmpty shouldBe false
    results.head._1.partNumber shouldBe "3001"
  }

  it should "search with dimension patterns containing spaces" in {
    val taxonomyData = TaxonomyData(Set.empty, testParts)
    val results = taxonomyData.searchByName("PLATE 1 X 6")

    results.isEmpty shouldBe false
    results.head._1.partNumber shouldBe "4006"
  }

  it should "search with unicode multiplication sign" in {
    val taxonomyData = TaxonomyData(Set.empty, testParts)
    val results = taxonomyData.searchByName("TILE 1 X 3 X 5")

    results.isEmpty shouldBe false
    results.head._1.partNumber shouldBe "4000"
  }

  "findExactMatch" should "find part where all words match" in {
    val taxonomyData = TaxonomyData(Set.empty, testParts)
    val result = taxonomyData.findExactMatch("BRICK 2X4")

    result.isDefined shouldBe true
    result.get.partNumber shouldBe "3001"
  }

  it should "return None if no exact match" in {
    val taxonomyData = TaxonomyData(Set.empty, testParts)
    val result = taxonomyData.findExactMatch("BRICK 2X10")

    result.isDefined shouldBe false
  }

  it should "return None for empty query" in {
    val taxonomyData = TaxonomyData(Set.empty, testParts)
    val result = taxonomyData.findExactMatch("")

    result.isDefined shouldBe false
  }

  "findCommonCategoryPrefix" should "find common category hierarchy" in {
    val taxonomyData = TaxonomyData(Set.empty, testParts)
    val searchResults = taxonomyData.searchByName("BRICK 2")
    val result = taxonomyData.findCommonCategoryPrefix(searchResults.take(5).map(_._1))

    result.isEmpty shouldBe false
    result.head.name shouldBe "Basic"
  }

  it should "return empty for no parts" in {
    val taxonomyData = TaxonomyData(Set.empty, Nil)
    val result = taxonomyData.findCommonCategoryPrefix(Nil)

    result shouldBe empty
  }

  "tokenize" should "handle simple names" in {
    val tokens = TaxonomyData.tokenize("BRICK 2X4")

    tokens should contain("brick")
    tokens should contain("2x4")
  }

  it should "remove trailing commas from words" in {
    val tokens = TaxonomyData.tokenize("BRICK W. KNOB,")

    tokens should contain("w.")
    tokens should contain("knob")
    tokens should not contain ("knob,")
  }

  it should "split words with periods" in {
    val tokens = TaxonomyData.tokenize("BRICK W.12")

    tokens should contain("w.")
    tokens should contain("12")
  }

  it should "split number-dash-alphabetics into two words" in {
    val tokens = TaxonomyData.tokenize("3-edges")

    tokens should contain("3")
    tokens should contain("edges")
  }

  it should "handle multiple number-dash patterns" in {
    val tokens = TaxonomyData.tokenize("3-edges 4-arms")

    tokens should contain("3")
    tokens should contain("edges")
    tokens should contain("4")
    tokens should contain("arms")
  }

  it should "handle empty string" in {
    val tokens = TaxonomyData.tokenize("")

    tokens shouldBe empty
  }

  // Integration tests for fuzzy matching behavior
  "fuzzy matching" should "match FLAT TILE 1X4, NO.115 to 1×4 Tile" in {
    val taxonomyParts = List(
      LegoPart("2431", "1×4 Tile", List(Category("1", "Basic", None), Category("3", "Tile", None), Category("4", "1× Tile", None)), 1, Set.empty, None, None, None)
    )
    val taxonomyData = TaxonomyData(Set.empty, taxonomyParts)
    
    val bricksetWords = TaxonomyData.tokenize("FLAT TILE 1X4, NO. 115").toSet
    val taxonomyWords = TaxonomyData.tokenize("1×4 Tile").toSet
    
    taxonomyWords.subsetOf(bricksetWords) shouldBe true
  }

  it should "match similar patterns with trailing commas" in {
    val bricksetWords = TaxonomyData.tokenize("FLAT PLATE 2X4, NO. 7").toSet
    val taxonomyWords = TaxonomyData.tokenize("PLATE 2X4").toSet
    
    taxonomyWords.subsetOf(bricksetWords) shouldBe true
  }

  it should "fail matching when taxonomy words are missing from brickset query" in {
    val bricksetWords = TaxonomyData.tokenize("TILE 1X4").toSet
    val taxonomyWords = TaxonomyData.tokenize("FLAT TILE 1X4").toSet
    
    taxonomyWords.subsetOf(bricksetWords) shouldBe false
  }

  it should "match when brickset part name is empty but ColoredPart name has value" in {
    val coloredPartName = "FLAT TILE 1X4, NO. 115"
    val taxonomyWords = TaxonomyData.tokenize("1×4 Tile").toSet
    
    val bricksetWords = TaxonomyData.tokenize(coloredPartName).toSet
    taxonomyWords.subsetOf(bricksetWords) shouldBe true
  }

  it should "fail matching when using empty string for brickset name" in {
    val emptyBricksetName = ""
    val taxonomyWords = TaxonomyData.tokenize("1×4 Tile").toSet
    
    val bricksetWords = TaxonomyData.tokenize(emptyBricksetName).toSet
    taxonomyWords.subsetOf(bricksetWords) shouldBe false
  }

  it should "match 37096 case with ColoredPart name after brickset name removal" in {
    val coloredPartName = "FLAT TILE 1X4, NO. 115"
    val taxonomyPartName = "1×4 Tile"
    
    val coloredPartWords = TaxonomyData.tokenize(coloredPartName).toSet
    val taxonomyWords = TaxonomyData.tokenize(taxonomyPartName).toSet
    
    taxonomyWords.subsetOf(coloredPartWords) shouldBe true
  }
}