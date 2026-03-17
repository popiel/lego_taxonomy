package com.example

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StudioIoReaderSpec extends AnyFlatSpec with Matchers {
  
  "StudioIoReader" should "parse simple.io and return parts with quantities" in {
    val ioReader = new StudioIoReader()
    val ioParts = ioReader.readColoredParts("src/test/resources/simple.io")

    ioParts should not be empty
    ioParts.foreach { part =>
      part.partNumber should not be empty
      part.color should not be empty
      part.quantity should be > 0
    }
  }

  it should "parse simple.io with part 3010 having quantity 6" in {
    val ioReader = new StudioIoReader()
    val parts = ioReader.readColoredParts("src/test/resources/simple.io")
    
    val part3010 = parts.find(_.partNumber == "3010")
    part3010 shouldBe defined
    part3010.get.quantity shouldBe 6
  }

  it should "parse LDraw format from string" in {
    val ldrContent = 
      """1 1 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat
        |1 4 10 0 0 1 0 0 0 1 0 0 0 1 3001.dat
        |11 7 3 0 0 0 0 0 0 1 0 0 0 1 0 3010.dat""".stripMargin
    
    val ioReader = new StudioIoReader()
    val parts = ioReader.readColoredPartsFromString(ldrContent)
    
    parts.length shouldBe 3
    parts.find(p => p.partNumber == "3001" && p.color == "Blue").get.quantity shouldBe 1
    parts.find(p => p.partNumber == "3001" && p.color == "Red").get.quantity shouldBe 1
    parts.find(p => p.partNumber == "3010" && p.color == "Blue").get.quantity shouldBe 3
  }
}
