package com.wolfskeep

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.io.{BufferedInputStream, FileInputStream}
import java.util.zip.ZipInputStream
import scala.io.Source

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
        |11 86 3 0 0 0 0 0 0 1 0 0 0 1 0 3010.dat""".stripMargin
    
    val ioReader = new StudioIoReader()
    val parts = ioReader.readColoredPartsFromString(ldrContent)
    
    parts.length shouldBe 3
    parts.find(p => p.partNumber == "3001" && p.color == "White").get.quantity shouldBe 1
    parts.find(p => p.partNumber == "3001" && p.color == "Orange").get.quantity shouldBe 1
    parts.find(p => p.partNumber == "3010" && p.color == "Light Bluish Gray").get.quantity shouldBe 3
  }

  it should "parse simple.io from ZipInputStream" in {
    val file = new java.io.File("src/test/resources/simple.io")
    val bufferedStream = new BufferedInputStream(new FileInputStream(file), 4096)
    
    val zipStream = new ZipInputStream(bufferedStream)
    var ldrContent: String = null
    var entry = zipStream.getNextEntry()
    while (entry != null) {
      if (entry.getName == "model.ldr") {
        ldrContent = Source.fromInputStream(zipStream).mkString
      }
      entry = zipStream.getNextEntry()
    }
    
    val parts = new StudioIoReader().readColoredPartsFromString(ldrContent)
    parts.find(_.partNumber == "3010").get.quantity shouldBe 6
  }

  it should "extract part names from model2.ldr descriptions" in {
    val ioReader = new StudioIoReader()
    val parts = ioReader.readColoredParts("src/test/resources/simple.io")
    
    val part3001 = parts.find(_.partNumber == "3001")
    part3001 shouldBe defined
    part3001.get.name shouldBe "Brick 2 x 4"
    
    val part3010 = parts.find(_.partNumber == "3010")
    part3010 shouldBe defined
    part3010.get.name shouldBe "Brick 1 x 4"
  }

  it should "parse pacer_version_2.1_new_steps.io using first subfile name" in {
    val ioReader = new StudioIoReader()
    val parts = ioReader.readColoredParts("src/test/resources/pacer_version_2.1_new_steps.io")
    
    parts should not be empty
    parts.length should be > 20
    parts.foreach { part =>
      part.partNumber should not be empty
      part.quantity should be > 0
    }
  }

  it should "map BrickLink color 1 to White" in {
    val parts = new StudioIoReader().readColoredParts("src/test/resources/pacer_version_2.1_new_steps.io")
    parts.find(_.color == "White") should not be empty
  }

  it should "map BrickLink color 11 to Black" in {
    val parts = new StudioIoReader().readColoredParts("src/test/resources/pacer_version_2.1_new_steps.io")
    parts.find(_.color == "Black") should not be empty
  }

  it should "map BrickLink color 5 to Red" in {
    val parts = new StudioIoReader().readColoredParts("src/test/resources/simple.io")
    parts.find(_.color == "Red") should not be empty
  }

  it should "map BrickLink color 7 to Blue" in {
    val parts = new StudioIoReader().readColoredParts("src/test/resources/simple.io")
    parts.find(_.color == "Blue") should not be empty
  }

  it should "map BrickLink color 12 to Trans-Clear" in {
    val parts = new StudioIoReader().readColoredParts("src/test/resources/pacer_version_2.1_new_steps.io")
    parts.find(_.color == "Trans-Clear") should not be empty
  }

  it should "map BrickLink color 86 to Light Bluish Gray" in {
    val parts = new StudioIoReader().readColoredParts("src/test/resources/pacer_version_2.1_new_steps.io")
    parts.find(_.color == "Light Bluish Gray") should not be empty
  }

  it should "map BrickLink color 85 to Dark Bluish Gray" in {
    val parts = new StudioIoReader().readColoredParts("src/test/resources/pacer_version_2.1_new_steps.io")
    parts.find(_.color == "Dark Bluish Gray") should not be empty
  }
}
