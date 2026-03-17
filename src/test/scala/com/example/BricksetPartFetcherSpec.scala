package com.example

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BricksetPartFetcherSpec extends AnyFlatSpec with Matchers {

  "parsePartDetails" should "correctly parse part 69040" in {
    val html = scala.io.Source.fromResource("brickset/69040.html").mkString
    val result = BricksetPartFetcher.parsePartDetails("69040", html)

    result.isDefined shouldBe true
    val part = result.get
    part.partNumber shouldBe "69040"
    part.name shouldBe "PLATE 4X6 W.12 KNOBS, NO.7"
    part.imageUrl shouldBe Some("https://images.brickset.com/parts/6309250.jpg")
    part.categories shouldBe empty
  }

  it should "correctly parse part 69315" in {
    val html = scala.io.Source.fromResource("brickset/69315.html").mkString
    val result = BricksetPartFetcher.parsePartDetails("69315", html)

    result.isDefined shouldBe true
    val part = result.get
    part.partNumber shouldBe "69315"
    part.name shouldBe "BANNER W.3.18 STICK 3X8, NO. 6"
    part.imageUrl shouldBe Some("https://images.brickset.com/parts/6338238.jpg")
    part.categories shouldBe empty
  }

  it should "correctly parse part 78185" in {
    val html = scala.io.Source.fromResource("brickset/78185.html").mkString
    val result = BricksetPartFetcher.parsePartDetails("78185", html)

    result.isDefined shouldBe true
    val part = result.get
    part.partNumber shouldBe "78185"
    part.name shouldBe "MINI FIGURE TROPHY, NO. 56"
    part.imageUrl shouldBe Some("https://images.brickset.com/parts/6342715.jpg")
    part.categories shouldBe empty
  }

  it should "return None for empty HTML" in {
    val html = "<html><body></body></html>"
    val result = BricksetPartFetcher.parsePartDetails("12345", html)

    result shouldBe None
  }

  it should "return None for HTML without part data" in {
    val html = """
      <!DOCTYPE html>
      <html>
      <head><title>Test</title></head>
      <body>
        <div class="content">
          <section class="setlist">
          </section>
        </div>
      </body>
      </html>
    """
    val result = BricksetPartFetcher.parsePartDetails("99999", html)

    result shouldBe None
  }
}
