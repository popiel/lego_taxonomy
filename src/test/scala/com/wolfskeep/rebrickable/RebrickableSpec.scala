package com.wolfskeep.rebrickable

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RebrickableSpec extends AnyFlatSpec with Matchers {

  "Data" should "load all Rebrickable data files" in {
    val data = Data.load()
    data.colors should not be empty
    data.parts should not be empty
    data.elements should not be empty
    data.sets should not be empty
    data.inventories should not be empty
    data.inventoryParts should not be empty
  }

  "Color" should "parse colors.csv.zip" in {
    val colors = Color.fromZip()
    colors should not be empty
    val unknown = colors.find(_.id == -1)
    unknown shouldBe defined
    unknown.get.name shouldBe "[Unknown]"
    unknown.get.rgb shouldBe "0033B2"
    unknown.get.isTrans shouldBe false
  }

  it should "parse Dark Gray color" in {
    val colors = Color.fromZip()
    val gray = colors.find(_.id == 8)
    gray shouldBe defined
    gray.get.name shouldBe "Dark Gray"
    gray.get.rgb shouldBe "6D6E5C"
  }

  it should "have multiple colors" in {
    val colors = Color.fromZip()
    colors.length should be > 100
  }

  "Part" should "parse parts.csv.zip" in {
    val parts = Part.fromZip()
    parts should not be empty
  }

  it should "have parts with valid part numbers" in {
    val parts = Part.fromZip()
    parts.head.partNum should not be empty
  }

  it should "have many parts" in {
    val parts = Part.fromZip()
    parts.length should be > 50000
  }

  it should "parse sticker sheet part" in {
    val parts = Part.fromZip()
    val sticker = parts.find(_.partNum == "003381")
    sticker shouldBe defined
    sticker.get.name should include("Sticker Sheet")
  }

  "Element" should "parse elements.csv.zip" in {
    val elements = Element.fromZip()
    elements should not be empty
  }

  it should "have elements with valid element IDs" in {
    val elements = Element.fromZip()
    elements.head.elementId should be > 0L
  }

  it should "have many elements" in {
    val elements = Element.fromZip()
    elements.length should be > 100000
  }

  it should "parse element with design ID" in {
    val elements = Element.fromZip()
    val withDesign = elements.find(_.designId.isDefined)
    withDesign shouldBe defined
    withDesign.get.designId.get should be > 0
  }

  it should "parse element without design ID" in {
    val elements = Element.fromZip()
    val withoutDesign = elements.find(_.designId.isEmpty)
    withoutDesign shouldBe defined
  }

  "RebrickableSet" should "parse sets.csv.zip" in {
    val sets = RebrickableSet.fromZip()
    sets should not be empty
  }

  it should "have sets with valid set numbers" in {
    val sets = RebrickableSet.fromZip()
    sets.head.setNum should not be empty
  }

  it should "have many sets" in {
    val sets = RebrickableSet.fromZip()
    sets.length should be > 20000
  }

  it should "parse a known set" in {
    val sets = RebrickableSet.fromZip()
    val ninjago = sets.find(_.setNum == "0003977811-1")
    ninjago shouldBe defined
    ninjago.get.name should include("Ninjago")
    ninjago.get.year shouldBe 2022
  }

  it should "handle optional img_url" in {
    val sets = RebrickableSet.fromZip()
    val withImg = sets.find(_.imgUrl.isDefined)
    withImg shouldBe defined
  }

  "Inventory" should "parse inventories.csv.zip" in {
    val inventories = Inventory.fromZip()
    inventories should not be empty
  }

  it should "have inventories with valid IDs" in {
    val inventories = Inventory.fromZip()
    inventories.head.id should be > 0
  }

  it should "have many inventories" in {
    val inventories = Inventory.fromZip()
    inventories.length should be > 40000
  }

  it should "parse inventory with version" in {
    val inventories = Inventory.fromZip()
    inventories.head.version should be >= 1
  }

  "InventoryPart" should "parse inventory_parts.csv.zip" in {
    val inventoryParts = InventoryPart.fromZip()
    inventoryParts should not be empty
  }

  it should "have inventory parts with valid inventory IDs" in {
    val inventoryParts = InventoryPart.fromZip()
    inventoryParts.head.inventoryId should be > 0
  }

  it should "have many inventory parts" in {
    val inventoryParts = InventoryPart.fromZip()
    inventoryParts.length should be > 1000000
  }

  it should "parse inventory part with quantity" in {
    val inventoryParts = InventoryPart.fromZip()
    inventoryParts.head.quantity should be > 0
  }

  it should "parse inventory part with is_spare" in {
    val inventoryParts = InventoryPart.fromZip()
    val spareParts = inventoryParts.filter(_.isSpare)
    spareParts should not be empty
  }

  it should "handle optional img_url" in {
    val inventoryParts = InventoryPart.fromZip()
    val withImg = inventoryParts.find(_.imgUrl.isDefined)
    withImg shouldBe defined
  }

  "Data.parseCsvLine" should "parse simple CSV line" in {
    val fields = Data.parseCsvLine("a,b,c")
    fields shouldBe List("a", "b", "c")
  }

  it should "handle quoted fields with commas" in {
    val fields = Data.parseCsvLine("a,\"b,c\",d")
    fields shouldBe List("a", "b,c", "d")
  }

  it should "handle empty fields" in {
    val fields = Data.parseCsvLine("a,,c")
    fields shouldBe List("a", "", "c")
  }
}
