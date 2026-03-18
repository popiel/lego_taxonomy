package com.example

import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import scala.collection.mutable
import scala.collection.JavaConverters._

object TaxonomyParser {

  def enhancePart(legoPart: LegoPart, html: String): LegoPart = {
    val doc = Jsoup.parse(html)
    val altNums = doc.select("span.part_num").asScala
      .map(_.text().trim)
      .filter(_.nonEmpty)
      .filter(_ != legoPart.partNumber)
      .toSet
    legoPart.copy(altNumbers = altNums)
  }

  def getCategoryNumber(href: String): Option[String] = {
    val startIndex = href.indexOf("parts/category-")
    if (startIndex != -1) {
      val start = startIndex + "parts/category-".length
      if (start < href.length) {
        val end = href.indexOf("?", start)
        val number = if (end == -1) href.substring(start) else href.substring(start, end)
        Some(number)
      } else None
    } else None
  }

  def parseRootHtml(html: String): List[Category] = {
    val doc = Jsoup.parse(html)
    var categories = List[Category]()
    val inlineResults = doc.selectFirst("div.inlineresults")
    if (inlineResults != null) {
      val categoryDivs = inlineResults.select("div.categorylistitem").asScala
      for {
        div <- categoryDivs
        a = div.selectFirst("a[href^='https://brickarchitect.com/parts/category']")
        if a != null
        number <- getCategoryNumber(a.attr("href"))
      } {
        val name = a.text().trim
        categories = Category(number, name, None) :: categories
      }
    }
    categories
  }

  def parseCategoryHtml(url: String, html: String): (List[Category], List[LegoPart]) = {
    val doc = Jsoup.parse(html)
    val categories = mutable.Map[String, Category]()
    var categoryParents = List[Category]()
    var parts = List[LegoPart]()

    // Extract categories from navbar
    val navbar = doc.selectFirst("div.navbar")
    if (navbar != null) {
      val navbarAs = navbar.select("a[href^='https://brickarchitect.com/parts/category']").asScala
      for {
        a <- navbarAs
        number <- getCategoryNumber(a.attr("href"))
        name = a.text().trim
      } {
        val cat = Category(number, name, categoryParents.headOption)
        categories(number) = cat    
        categoryParents = cat :: categoryParents
      }
    }

    // Extract main category
    val mainNumberOpt = getCategoryNumber(url)
    val mainNumber = mainNumberOpt.getOrElse("1")
    val h1 = doc.selectFirst("div.main h1")
    val mainName = h1.ownText().trim()
    println(s"Main category: $mainNumber - $mainName from URL $url")
    val mainCat = Category(mainNumber, mainName, categoryParents.headOption)
    categories(mainNumber) = mainCat
    categoryParents = mainCat :: categoryParents

    // Extract child categories from div.part_category
    def extractChildCategories(element: Element, parent: Category): Unit = {
      for {
        div <- element.children().asScala
        if div.tagName() == "div" && div.classNames().contains("part_category")
        a = div.selectFirst("a[href^='https://brickarchitect.com/parts/category']")
        if a != null
        number <- getCategoryNumber(a.attr("href"))
      } {
        val name = a.text().trim
        val cat = Category(number, name, Some(parent))
        categories(number) = cat
        extractParts(div, cat)
        extractChildCategories(div, cat)
      }
    }

    // Extract parts from div.parts_results
    def extractParts(element: Element, category: Category): Unit = {
      var sequenceNumber = 0   
      for {
        results <- element.children().asScala
        if results.tagName() == "div" && results.classNames().contains("parts_results")
        span <- results.select("span.td.part_name").asScala
        partNumSpan = span.selectFirst("span.partnum")
        partNameSpan = span.selectFirst("span.partname")
        if partNumSpan != null && partNameSpan != null
      } {
        val partNumber = partNumSpan.text().trim
        val partName = partNameSpan.text().trim
        sequenceNumber += 1
        
        val imgElement = Option(span.parent())
          .flatMap(parent => Option(parent.selectFirst("span.td.part_image")))
          .flatMap(imgSpan => Option(imgSpan.selectFirst("img")))
        
        val imageUrl = imgElement.flatMap(img => Option(img.attr("src"))).filter(_.nonEmpty)
        val imageWidth = imgElement.flatMap(img => Option(img.attr("width"))).filter(_.nonEmpty)
        val imageHeight = imgElement.flatMap(img => Option(img.attr("height"))).filter(_.nonEmpty)
        
        val legoPart = LegoPart(partNumber, partName, category.hierarchy, sequenceNumber, 
          imageUrl = imageUrl, imageWidth = imageWidth, imageHeight = imageHeight)
        parts = legoPart :: parts
      }
    }

    val inlineResults = doc.selectFirst("div.inlineresults")
    extractParts(inlineResults, mainCat)
    extractChildCategories(inlineResults, mainCat)
    (categories.values.toList, parts)
  }
}