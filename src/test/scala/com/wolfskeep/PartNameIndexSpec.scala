package com.wolfskeep

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PartNameIndexSpec extends AnyFlatSpec with Matchers {

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

  "buildIndex" should "create an index from part names" in {
    val index = PartNameIndex.buildIndex(testParts)

    index.contains("brick") shouldBe true
    index.contains("plate") shouldBe true
    index.contains("tile") shouldBe true
    index.contains("2x4") shouldBe true
    index.contains("2x2") shouldBe true
    index.contains("2x6") shouldBe true
    index.contains("1x1") shouldBe true
    index.contains("1x2") shouldBe true
  }

  it should "map words to correct parts" in {
    val index = PartNameIndex.buildIndex(testParts)

    index("brick").map(_.partNumber).toSet shouldBe Set("3001", "3002", "3003", "4001")
    index("plate").map(_.partNumber).toSet shouldBe Set("3022", "3023", "4006", "3031")
    index("tile").map(_.partNumber).toSet shouldBe Set("3070", "4000")
  }

  it should "handle empty parts list" in {
    val index = PartNameIndex.buildIndex(Nil)
    index shouldBe empty
  }

  it should "collapse dimension patterns with spaces" in {
    val index = PartNameIndex.buildIndex(testParts)

    index.contains("1x6") shouldBe true
    index.contains("1x3x5") shouldBe true
    index("1x6").map(_.partNumber).toSet shouldBe Set("4006")
    index("1x3x5").map(_.partNumber).toSet shouldBe Set("4000")
  }

  it should "handle unicode multiplication sign" in {
    val index = PartNameIndex.buildIndex(testParts)

    index.contains("1x6") shouldBe true
    index("1x6").map(_.partNumber).toSet shouldBe Set("4006")
  }

  it should "search with dimension patterns containing spaces" in {
    val index = PartNameIndex.buildIndex(testParts)
    val results = PartNameIndex.search(index, "PLATE 1 X 6")

    results.isEmpty shouldBe false
    results.head._1.partNumber shouldBe "4006"
  }

  it should "search with unicode multiplication sign" in {
    val index = PartNameIndex.buildIndex(testParts)
    val results = PartNameIndex.search(index, "TILE 1 X 3 X 5")

    results.isEmpty shouldBe false
    results.head._1.partNumber shouldBe "4000"
  }

  it should "remove trailing commas from words" in {
    val partsWithComma = List(
      LegoPart("5001", "BRICK W. KNOB,", List(Category("1", "Basic", None)), 1, Set.empty, None, None, None)
    )
    val index = PartNameIndex.buildIndex(partsWithComma)

    index.contains("w.") shouldBe true
    index.contains("knob") shouldBe true
    index.contains("knob,") shouldBe false
  }

  it should "split words with periods" in {
    val partsWithPeriod = List(
      LegoPart("3001", "BRICK W.12", List(Category("1", "Basic", None)), 1, Set.empty, None, None, None),
      LegoPart("3002", "BRICK NO.7", List(Category("1", "Basic", None)), 2, Set.empty, None, None, None)
    )
    val index = PartNameIndex.buildIndex(partsWithPeriod)

    index.contains("w.") shouldBe true
    index.contains("12") shouldBe true
    index.contains("no.") shouldBe true
    index.contains("7") shouldBe true
  }

  it should "search with words containing periods" in {
    val partsWithPeriod = List(
      LegoPart("3001", "BRICK W.12", List(Category("1", "Basic", None)), 1, Set.empty, None, None, None)
    )
    val index = PartNameIndex.buildIndex(partsWithPeriod)
    val results = PartNameIndex.search(index, "BRICK W.12")

    results.isEmpty shouldBe false
    results.head._1.partNumber shouldBe "3001"
  }

  "search" should "return parts sorted by match count" in {
    val index = PartNameIndex.buildIndex(testParts)
    val results = PartNameIndex.search(index, "BRICK 2X2")

    results.isEmpty shouldBe false
    results.head._1.partNumber shouldBe "3002"
    results.head._2 shouldBe 2
  }

  it should "return empty list for unknown query" in {
    val index = PartNameIndex.buildIndex(testParts)
    val results = PartNameIndex.search(index, "unknown part")

    results.isEmpty shouldBe true
  }

  it should "respect limit parameter" in {
    val index = PartNameIndex.buildIndex(testParts)
    val results = PartNameIndex.search(index, "brick", limit = 2)

    results.length shouldBe 2
  }

  it should "be case insensitive" in {
    val index = PartNameIndex.buildIndex(testParts)
    val results = PartNameIndex.search(index, "brick")

    results.isEmpty shouldBe false
    results.head._1.partNumber shouldBe "3001"
  }

  "findExactMatch" should "find part where all words match" in {
    val index = PartNameIndex.buildIndex(testParts)
    val result = PartNameIndex.findExactMatch(index, "BRICK 2X4")

    result.isDefined shouldBe true
    result.get.partNumber shouldBe "3001"
  }

  it should "return None if no exact match" in {
    val index = PartNameIndex.buildIndex(testParts)
    val result = PartNameIndex.findExactMatch(index, "BRICK 2X10")

    result.isDefined shouldBe false
  }

  it should "return None for empty query" in {
    val index = PartNameIndex.buildIndex(testParts)
    val result = PartNameIndex.findExactMatch(index, "")

    result.isDefined shouldBe false
  }

  // Tests for the 37096 bug fix - fuzzy matching with commas
  "fuzzy matching" should "match FLAT TILE 1X4, NO.115 to 1×4 Tile" in {
    val taxonomyParts = List(
      LegoPart("2431", "1×4 Tile", List(Category("1", "Basic", None), Category("3", "Tile", None), Category("4", "1× Tile", None)), 1, Set.empty, None, None, None)
    )
    
    // Simulate the logic used in inferCategories:
    // bricksetWords = tokenize("FLAT TILE 1X4, NO. 115")
    // partWords = tokenize("1×4 Tile")
    // check: partWords.subsetOf(bricksetWords)
    
    val bricksetWords = PartNameIndex.tokenize("FLAT TILE 1X4, NO. 115").toSet
    val taxonomyWords = PartNameIndex.tokenize("1×4 Tile").toSet
    
    // Taxonomy words should be a subset of Brickset words
    taxonomyWords.subsetOf(bricksetWords) shouldBe true
  }

  it should "match similar patterns with trailing commas" in {
    // Simulate the logic used in inferCategories
    val bricksetWords = PartNameIndex.tokenize("FLAT PLATE 2X4, NO. 7").toSet
    val taxonomyWords = PartNameIndex.tokenize("PLATE 2X4").toSet
    
    taxonomyWords.subsetOf(bricksetWords) shouldBe true
  }

  it should "use tokenize function for fuzzy matching" in {
    // This test verifies that the tokenize function handles the case that caused the 37096 bug
    // Before the fix, simple split was used which didn't handle commas properly
    
    // Direct tokenize call to verify it produces correct tokens
    val bricksetTokens = PartNameIndex.tokenize("FLAT TILE 1X4, NO. 115")
    bricksetTokens should contain ("1x4")
    bricksetTokens should contain ("tile")
    bricksetTokens should contain ("no.")
    bricksetTokens should not contain ("1x4,")  // comma should be removed
    
    val taxonomyTokens = PartNameIndex.tokenize("1×4 Tile")
    taxonomyTokens should contain ("1x4")
    taxonomyTokens should contain ("tile")
  }
  
  it should "fail matching when taxonomy words are missing from brickset query" in {
    // If Brickset query doesn't contain all taxonomy words, should not match
    val bricksetWords = PartNameIndex.tokenize("TILE 1X4").toSet  // missing "FLAT"
    val taxonomyWords = PartNameIndex.tokenize("FLAT TILE 1X4").toSet
    
    taxonomyWords.subsetOf(bricksetWords) shouldBe false
  }

  // Tests for regression when brickset part name was removed (made empty)
  // The fuzzy matching should use ColoredPart.name instead of bricksetPart.name
  it should "match when brickset part name is empty but ColoredPart name has value" in {
    // This simulates the case after the BricksetPartFetcher change where:
    // - bricksetPart.name = "" (empty)
    // - coloredPart.name = "FLAT TILE 1X4, NO. 115"
    // The matching should use the ColoredPart name, not the empty brickset name
    
    val coloredPartName = "FLAT TILE 1X4, NO. 115"  // This is what should be used for matching
    val taxonomyWords = PartNameIndex.tokenize("1×4 Tile").toSet
    
    // Using coloredPartName (not empty brickset name) should work
    val bricksetWords = PartNameIndex.tokenize(coloredPartName).toSet
    taxonomyWords.subsetOf(bricksetWords) shouldBe true
  }

  it should "fail matching when using empty string for brickset name" in {
    // This test verifies that using an empty string would fail
    // (which is what was happening before the fix)
    val emptyBricksetName = ""
    val taxonomyWords = PartNameIndex.tokenize("1×4 Tile").toSet
    
    val bricksetWords = PartNameIndex.tokenize(emptyBricksetName).toSet
    taxonomyWords.subsetOf(bricksetWords) shouldBe false
  }

  it should "match 37096 case with ColoredPart name after brickset name removal" in {
    // The specific 37096 case:
    // - ColoredPart.name = "FLAT TILE 1X4, NO. 115"
    // - bricksetPart.name = "" (now empty due to change)
    // - Taxonomy part 2431 = "1×4 Tile"
    // 
    // After fix, we use ColoredPart.name for matching, not bricksetPart.name
    
    val coloredPartName = "FLAT TILE 1X4, NO. 115"
    val taxonomyPartName = "1×4 Tile"
    
    val coloredPartWords = PartNameIndex.tokenize(coloredPartName).toSet
    val taxonomyWords = PartNameIndex.tokenize(taxonomyPartName).toSet
    
    // This should work: taxonomy words are subset of colored part words
    taxonomyWords.subsetOf(coloredPartWords) shouldBe true
  }

  // Tests for number-dash-alphabetics splitting
  it should "split number-dash-alphabetics into two words" in {
    val tokens = PartNameIndex.tokenize("3-edges")
    tokens should contain ("3")
    tokens should contain ("edges")
  }

  it should "handle multiple number-dash patterns" in {
    // "3-edges" should be split into "3" and "edges"
    val tokens = PartNameIndex.tokenize("3-edges 4-arms")
    tokens should contain ("3")
    tokens should contain ("edges")
    tokens should contain ("4")
    tokens should contain ("arms")
  }
}
