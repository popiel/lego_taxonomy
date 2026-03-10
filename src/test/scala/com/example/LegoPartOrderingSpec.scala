package com.example

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LegoPartOrderingSpec extends AnyFlatSpec with Matchers {
  "LegoPart ordering" should "order by category hierarchy" in {
    val cat1 = Category("1", "A", None)
    val cat1a = Category("1", "A", Some(cat1)) // same number, parent irrelevant
    val cat2 = Category("2", "B", None)

    val p1 = LegoPart("200", "First", List(cat1))
    val p2 = LegoPart("100", "Second", List(cat2))

    // p1 has category 1 and p2 has category 2 -> p1 < p2
    (p1 < p2) shouldBe true
    (p2 < p1) shouldBe false
  }

  it should "consider deeper category lists" in {
    val root = Category("1", "Root", None)
    val child = Category("2", "Child", Some(root))
    val leaf = Category("3", "Leaf", Some(child))

    val pRoot = LegoPart("1", "RootPart", List(root))
    val pLeaf = LegoPart("2", "LeafPart", List(root, child, leaf))

    (pRoot < pLeaf) shouldBe true
    (pLeaf < pRoot) shouldBe false
  }

  it should "fall back to sequenceNumber when categories compare equal" in {
    val c = Category("5", "Same", None)
    val a = LegoPart("20", "A", List(c), 1)
    val b = LegoPart("10", "B", List(c), 2)
    (a < b) shouldBe true
    (b < a) shouldBe false
  }

  it should "fall back to partNumber when categories and sequenceNumber are equal" in {
    val c = Category("5", "Same", None)
    val a = LegoPart("10", "A", List(c), 1)
    val b = LegoPart("20", "B", List(c), 1)
    (a < b) shouldBe true
    (b < a) shouldBe false
  }
}
