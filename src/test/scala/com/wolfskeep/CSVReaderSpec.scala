package com.wolfskeep

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.wolfskeep.rebrickable.Data

class CSVReaderSpec extends AnyFlatSpec with Matchers {

  "CsvReader" should "read simple.csv correctly" in {
    val reader = new CsvReader()
    val parts = reader.readColoredParts("src/test/resources/simple.csv")
    parts.size should be (3)
    parts.count(_.color == "Red") should be (2)
    parts.count(_.partNumber == "3001") should be (2)
    parts.foreach(_.elementId should not be empty)
    val blue3001 = parts.find(p => p.partNumber == "3001" && p.color == "Blue")
    blue3001.get.elementId shouldBe Some("300123")
  }

  it should "read Brickset-inventory-21321-1.csv correctly" in {
    val reader = new CsvReader()
    val parts = reader.readColoredParts("src/test/resources/Brickset-inventory-21321-1.csv")
    parts.size should be (187)
    parts.count(_.color == "Black") should be > 0
    parts.count(_.partNumber == "2412") should be (1)
    parts.find(_.partNumber == "2412").get.elementId shouldBe Some("241226")
  }

  it should "read rebrickable format CSV with color conversion and Is Spare filtering" in {
    val colorIdToName = Map(0 -> "Black", 72 -> "Dark Bluish Gray", 1 -> "Blue")
    val reader = new CsvReader()
    val parts = reader.readColoredParts("src/test/resources/rebrickable_parts_moc-257369-sherlock-holmes-baker-street.csv", colorIdToName)
    
    parts should not be empty
    
    val blackParts = parts.filter(_.color == "Black")
    blackParts should not be empty
    
    val darkBluishGrayParts = parts.filter(_.color == "Dark Bluish Gray")
    darkBluishGrayParts should not be empty
    
    val unknownColorParts = parts.filter(_.color.startsWith("unknown-"))
    unknownColorParts should not be empty
    
    val numericColorParts = parts.filter(p => p.color.matches("\\d+"))
    numericColorParts shouldBe empty
  }

  it should "read element-only CSV and resolve elementId to part number and color" in {
    val data = Data.load()
    val colorIdToName = data.colors.map(c => c.id -> c.name).toMap
    val elementIdToPartColor = data.elements.map { e =>
      val colorName = data.colorIdToColor.get(e.colorId).map(_.name).getOrElse(s"unknown-${e.colorId}")
      val partName = data.partNumToPart.get(e.partNum).map(_.name).getOrElse("")
      e.elementId -> (e.partNum, colorName, partName)
    }.toMap
    
    val reader = new CsvReader()
    val parts = reader.readColoredParts(
      "src/test/resources/lego_pab_parts_moc-257369-sherlock-holmes-baker-street.csv",
      colorIdToName,
      elementIdToPartColor
    )
    
    parts should not be empty
    
    parts.foreach { part =>
      part.partNumber should not be empty
      part.color should not be empty
      part.elementId should not be empty
    }
    
    val unknownParts = parts.filter(p => p.partNumber.startsWith("unknown-element-") || p.color.startsWith("unknown-"))
    unknownParts shouldBe empty
  }

  it should "produce same parts from rebrickable format and element-only format" in {
    val data = Data.load()
    val colorIdToName = data.colors.map(c => c.id -> c.name).toMap
    val elementIdToPartColor = data.elements.map { e =>
      val colorName = data.colorIdToColor.get(e.colorId).map(_.name).getOrElse(s"unknown-${e.colorId}")
      val partName = data.partNumToPart.get(e.partNum).map(_.name).getOrElse("")
      e.elementId -> (e.partNum, colorName, partName)
    }.toMap
    
    val reader = new CsvReader()
    
    val rebrickableParts = reader.readColoredParts(
      "src/test/resources/rebrickable_parts_moc-257369-sherlock-holmes-baker-street.csv",
      colorIdToName
    )
    
    val elementOnlyParts = reader.readColoredParts(
      "src/test/resources/lego_pab_parts_moc-257369-sherlock-holmes-baker-street.csv",
      colorIdToName,
      elementIdToPartColor
    )
    
    rebrickableParts.size shouldBe elementOnlyParts.size
    
    val rebrickableByKey = rebrickableParts.map(p => (p.partNumber, p.color) -> p).sortBy(_._1.toString)
    val elementOnlyByKey = elementOnlyParts.map(p => (p.partNumber, p.color) -> p).sortBy(_._1.toString)
    
    rebrickableByKey.zip(elementOnlyByKey).foreach { case ((key1, p1), (key2, p2)) =>
      key1 shouldBe key2
      p1.quantity shouldBe p2.quantity
    }
  }
}