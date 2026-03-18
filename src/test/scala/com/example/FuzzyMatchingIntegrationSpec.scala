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
    
    val taxonomyWords = PartNameIndex.tokenize(matchedPart.name).toSet
    val coloredPartWords = PartNameIndex.tokenize(coloredPartName).toSet
    
    val useTaxonomyName = taxonomyWords.subsetOf(coloredPartWords) &&
                          matchedPart.categories == searchResults.head._1.categories
    
    val newName = if (useTaxonomyName) s"${matchedPart.name} (guessed)" else bricksetPart.name
    
    newName shouldBe "1×4 Tile (guessed)"
  }
}
