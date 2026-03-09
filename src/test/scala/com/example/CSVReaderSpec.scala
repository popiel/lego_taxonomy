package com.example

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CSVReaderSpec extends AnyFlatSpec with Matchers {

  "CsvReader" should "read simple.csv correctly" in {
    val reader = new CsvReader()
    val parts = reader.readColoredParts("src/test/resources/simple.csv")
    parts.size should be (3)
    parts.count(_.color == "Red") should be (2)
    parts.count(_.partNumber == "3001") should be (2)
  }

  it should "read Brickset-inventory-21321-1.csv correctly" in {
    val reader = new CsvReader()
    val parts = reader.readColoredParts("src/test/resources/Brickset-inventory-21321-1.csv")
    parts.size should be (187)
    parts.count(_.color == "Black") should be > 0
    parts.count(_.partNumber == "2412") should be (1)
  }
}