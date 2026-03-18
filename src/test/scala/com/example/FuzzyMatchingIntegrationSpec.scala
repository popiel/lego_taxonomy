package com.example

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FuzzyMatchingIntegrationSpec extends AnyFlatSpec with Matchers {

  "Fuzzy matching with ColoredPart name" should "match FLAT TILE 1X4, NO. 115 to 1×4 Tile using ColoredPart name" in {
    val taxonomyParts = List(
      LegoPart("2431", "1×4 Tile", List(Category("1", "Basic", None), Category("3", "Tile", None), Category("4", "1× Tile", None)), 1, Set.empty, None, None, None)
    )
    val index = PartNameIndex.buildIndex(taxonomyParts)
    
    val searchResults = PartNameIndex.search(index, "FLAT TILE 1X4, NO. 115")
    searchResults.isEmpty shouldBe false
    
    val bricksetPart = LegoPart(
      partNumber = "",
      name = "",
      categories = Nil,
      sequenceNumber = 0,
      altNumbers = Set.empty,
      imageUrl = Some("https://example.com/37096.jpg"),
      imageWidth = None,
      imageHeight = None
    )
    
    val coloredPart = ColoredPart(
      partNumber = "37096",
      name = "FLAT TILE 1X4, NO. 115",
      color = "Earth Blue",
      quantity = 1
    )
    
    val matchedPart = MatchedPart(coloredPart, Some(bricksetPart))
    
    val bricksetWords = PartNameIndex.tokenize(coloredPart.name).toSet
    
    val exactMatch = searchResults.find { case (part, _) =>
      val partWords = PartNameIndex.tokenize(part.name).toSet
      partWords.subsetOf(bricksetWords)
    }
    
    exactMatch.isDefined shouldBe true
    exactMatch.get._1.partNumber shouldBe "2431"
    
    val updatedPart = bricksetPart.copy(categories = exactMatch.get._1.categories)
    updatedPart.categories.isEmpty shouldBe false
    updatedPart.categories.map(_.name) should contain("Tile")
  }
  
  it should "use taxonomy part name with (guessed) suffix when exact match found" in {
    val taxonomyParts = List(
      LegoPart("2431", "1×4 Tile", List(Category("1", "Basic", None), Category("3", "Tile", None), Category("4", "1× Tile", None)), 1, Set.empty, None, None, None)
    )
    val index = PartNameIndex.buildIndex(taxonomyParts)
    
    val searchResults = PartNameIndex.search(index, "FLAT TILE 1X4, NO. 115")
    
    val bricksetPart = LegoPart(
      partNumber = "37096",
      name = "",
      categories = Nil,
      sequenceNumber = 0,
      altNumbers = Set.empty,
      imageUrl = Some("https://example.com/37096.jpg"),
      imageWidth = None,
      imageHeight = None
    )
    
    val coloredPartName = "FLAT TILE 1X4, NO. 115"
    val bricksetWords = PartNameIndex.tokenize(coloredPartName).toSet
    
    val exactMatch = searchResults.find { case (part, _) =>
      val partWords = PartNameIndex.tokenize(part.name).toSet
      partWords.subsetOf(bricksetWords)
    }
    
    exactMatch.isDefined shouldBe true
    val matchedPart = exactMatch.get._1
    
    val newName = s"${matchedPart.name} (guessed)"
    
    newName shouldBe "1×4 Tile (guessed)"
  }
  
  it should "use taxonomy name with (guessed) for common prefix match" in {
    val taxonomyParts = List(
      LegoPart("3001", "2×4 Brick", List(Category("1", "Basic", None), Category("2", "Brick", None)), 1, Set.empty, None, None, None),
      LegoPart("3003", "2×3 Brick", List(Category("1", "Basic", None), Category("2", "Brick", None)), 2, Set.empty, None, None, None),
      LegoPart("3004", "1×2 Brick", List(Category("1", "Basic", None), Category("2", "Brick", None)), 3, Set.empty, None, None, None)
    )
    val index = PartNameIndex.buildIndex(taxonomyParts)
    
    val searchResults = PartNameIndex.search(index, "BRICK 2")
    
    val commonPrefix = PartNameIndex.findCommonCategoryPrefix(searchResults.take(5).map(_._1))
    commonPrefix.nonEmpty shouldBe true
    
    val bestMatch = searchResults.take(5).find { case (part, _) =>
      part.categories.zip(commonPrefix).forall { case (cat, prefixCat) => cat == prefixCat }
    }
    
    bestMatch.isDefined shouldBe true
    val newName = bestMatch.map(p => s"${p._1.name} (guessed)").getOrElse("")
    
    newName should include("(guessed)")
  }
  
  it should "guess name for part 39746 from Brickset inventory using taxonomy" in {
    val taxonomyParts = List(
      LegoPart("2470", "2×2 Round Tile", List(Category("1", "Basic", None), Category("3", "Tile", None), Category("4", "Round Tile", None)), 1, Set.empty, None, None, None),
      LegoPart("2920", "1×1 Round Tile", List(Category("1", "Basic", None), Category("3", "Tile", None), Category("4", "Round Tile", None)), 2, Set.empty, None, None, None),
      LegoPart("2431", "1×4 Tile", List(Category("1", "Basic", None), Category("3", "Tile", None), Category("4", "1× Tile", None)), 3, Set.empty, None, None, None)
    )
    val index = PartNameIndex.buildIndex(taxonomyParts)
    
    val coloredPartName = "FLAT TILE 2X2, ROUND, NO. 1112"
    val searchResults = PartNameIndex.search(index, coloredPartName)
    
    searchResults.isEmpty shouldBe false
    
    val bricksetPart = LegoPart(
      partNumber = "39746",
      name = "",
      categories = Nil,
      sequenceNumber = 0,
      altNumbers = Set.empty,
      imageUrl = Some("https://example.com/39746.jpg"),
      imageWidth = None,
      imageHeight = None
    )
    
    val bricksetWords = PartNameIndex.tokenize(coloredPartName).toSet
    
    val exactMatch = searchResults.find { case (part, _) =>
      val partWords = PartNameIndex.tokenize(part.name).toSet
      partWords.subsetOf(bricksetWords)
    }
    
    exactMatch.isDefined shouldBe true
    val matchedPart = exactMatch.get._1
    
    val newName = s"${matchedPart.name} (guessed)"
    
    newName shouldBe "2×2 Round Tile (guessed)"
    newName should include("(guessed)")
  }
}
