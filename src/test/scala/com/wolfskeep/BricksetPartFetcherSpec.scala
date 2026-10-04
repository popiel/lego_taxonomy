package com.wolfskeep

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.concurrent.Await
import scala.concurrent.duration._

class BricksetPartFetcherSpec extends AnyFlatSpec with Matchers {

  "BricksetPartFetcher" should "parse part details and extract element number from HTML" in {
    val html = scala.io.Source.fromResource("brickset/69038.html").mkString
    val result = Await.result(
      BricksetPartFetcher.parsePartDetails("69038", None, html),
      1.seconds
    )
    
    result shouldBe defined
    result.get.partNumber shouldBe "69038"
    result.get.elementId shouldBe Some("6331694")
    result.get.imageUrl shouldBe defined
    result.get.imageUrl.get should include ("6331694.jpg")
  }

  it should "prefer elementId from CSV over HTML extraction" in {
    val html = scala.io.Source.fromResource("brickset/69038.html").mkString
    val result = Await.result(
      BricksetPartFetcher.parsePartDetails("69038", Some("123456"), html),
      1.seconds
    )
    
    result shouldBe defined
    result.get.elementId shouldBe Some("123456")
  }

  it should "return None for HTML without article" in {
    val html = "<html><body><p>No article</p></body></html>"
    val result = Await.result(
      BricksetPartFetcher.parsePartDetails("12345", None, html),
      1.seconds
    )
    
    result shouldBe None
  }

  it should "return None for HTML without element number or image" in {
    val html = """<html><body><article class="set"></article></body></html>"""
    val result = Await.result(
      BricksetPartFetcher.parsePartDetails("12345", None, html),
      1.seconds
    )
    
    result shouldBe None
  }
}
