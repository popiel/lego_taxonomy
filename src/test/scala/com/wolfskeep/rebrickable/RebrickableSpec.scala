package com.wolfskeep.rebrickable

import org.scalatest.wordspec.AnyWordSpec

class RebrickableSpec extends AnyWordSpec {

  "Data" should {
    "load all Rebrickable data files" in {
      val data = Data.load()
      assert(data.colors.nonEmpty, "colors should not be empty")
      assert(data.parts.nonEmpty, "parts should not be empty")
      assert(data.elements.nonEmpty, "elements should not be empty")
      assert(data.sets.nonEmpty, "sets should not be empty")
      assert(data.inventories.nonEmpty, "inventories should not be empty")
      assert(data.inventoryParts.nonEmpty, "inventoryParts should not be empty")
    }
  }

  "Color" should {
    "parse colors.csv.zip" in {
      val colors = Color.fromZip()
      assert(colors.nonEmpty)
      val unknown = colors.find(_.id == -1)
      assert(unknown.isDefined)
      assert(unknown.get.name == "[Unknown]")
      assert(unknown.get.rgb == "0033B2")
      assert(unknown.get.isTrans == false)
    }

    "parse Black color" in {
      val colors = Color.fromZip()
      val black = colors.find(_.id == 0)
      assert(black.isDefined)
      assert(black.get.name == "Black")
      assert(black.get.rgb == "05131D")
      assert(black.get.yearFrom == 1957)
      assert(black.get.yearTo == 2026)
    }

    "have multiple colors" in {
      val colors = Color.fromZip()
      assert(colors.length > 100)
    }
  }

  "Part" should {
    "parse parts.csv.zip" in {
      val parts = Part.fromZip()
      assert(parts.nonEmpty)
    }

    "have parts with valid part numbers" in {
      val parts = Part.fromZip()
      assert(parts.head.partNum.nonEmpty)
    }

    "have many parts" in {
      val parts = Part.fromZip()
      assert(parts.length > 50000)
    }

    "parse sticker sheet part" in {
      val parts = Part.fromZip()
      val sticker = parts.find(_.partNum == "003381")
      assert(sticker.isDefined)
      assert(sticker.get.name.contains("Sticker Sheet"))
    }
  }

  "Element" should {
    "parse elements.csv.zip" in {
      val elements = Element.fromZip()
      assert(elements.nonEmpty)
    }

    "have elements with valid element IDs" in {
      val elements = Element.fromZip()
      assert(elements.head.elementId > 0)
    }

    "have many elements" in {
      val elements = Element.fromZip()
      assert(elements.length > 100000)
    }

    "parse element with design ID" in {
      val elements = Element.fromZip()
      val withDesign = elements.find(_.designId.isDefined)
      assert(withDesign.isDefined)
      assert(withDesign.get.designId.get > 0)
    }

    "parse element without design ID" in {
      val elements = Element.fromZip()
      val withoutDesign = elements.find(_.designId.isEmpty)
      assert(withoutDesign.isDefined)
    }
  }

  "RebrickableSet" should {
    "parse sets.csv.zip" in {
      val sets = RebrickableSet.fromZip()
      assert(sets.nonEmpty)
    }

    "have sets with valid set numbers" in {
      val sets = RebrickableSet.fromZip()
      assert(sets.head.setNum.nonEmpty)
    }

    "have many sets" in {
      val sets = RebrickableSet.fromZip()
      assert(sets.length > 20000)
    }

    "parse a known set" in {
      val sets = RebrickableSet.fromZip()
      val ninjago = sets.find(_.setNum == "0003977811-1")
      assert(ninjago.isDefined)
      assert(ninjago.get.name.contains("Ninjago"))
      assert(ninjago.get.year == 2022)
    }

    "handle optional img_url" in {
      val sets = RebrickableSet.fromZip()
      val withImg = sets.find(_.imgUrl.isDefined)
      assert(withImg.isDefined)
    }
  }

  "Inventory" should {
    "parse inventories.csv.zip" in {
      val inventories = Inventory.fromZip()
      assert(inventories.nonEmpty)
    }

    "have inventories with valid IDs" in {
      val inventories = Inventory.fromZip()
      assert(inventories.head.id > 0)
    }

    "have many inventories" in {
      val inventories = Inventory.fromZip()
      assert(inventories.length > 40000)
    }

    "parse inventory with version" in {
      val inventories = Inventory.fromZip()
      val inv = inventories.head
      assert(inv.version >= 1)
    }
  }

  "InventoryPart" should {
    "parse inventory_parts.csv.zip" in {
      val inventoryParts = InventoryPart.fromZip()
      assert(inventoryParts.nonEmpty)
    }

    "have inventory parts with valid inventory IDs" in {
      val inventoryParts = InventoryPart.fromZip()
      assert(inventoryParts.head.inventoryId > 0)
    }

    "have many inventory parts" in {
      val inventoryParts = InventoryPart.fromZip()
      assert(inventoryParts.length > 1000000)
    }

    "parse inventory part with quantity" in {
      val inventoryParts = InventoryPart.fromZip()
      val part = inventoryParts.head
      assert(part.quantity > 0)
    }

    "parse inventory part with is_spare" in {
      val inventoryParts = InventoryPart.fromZip()
      val spareParts = inventoryParts.filter(_.isSpare)
      assert(spareParts.nonEmpty)
    }

    "handle optional img_url" in {
      val inventoryParts = InventoryPart.fromZip()
      val withImg = inventoryParts.find(_.imgUrl.isDefined)
      assert(withImg.isDefined)
    }
  }

  "Data.parseCsvLine" should {
    "parse simple CSV line" in {
      val fields = Data.parseCsvLine("a,b,c")
      assert(fields == List("a", "b", "c"))
    }

    "handle quoted fields with commas" in {
      val fields = Data.parseCsvLine("a,\"b,c\",d")
      assert(fields == List("a", "b,c", "d"))
    }

    "handle empty fields" in {
      val fields = Data.parseCsvLine("a,,c")
      assert(fields == List("a", "", "c"))
    }
  }
}
