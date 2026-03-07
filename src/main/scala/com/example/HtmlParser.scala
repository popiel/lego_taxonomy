package com.example

import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import scala.collection.mutable
import scala.collection.JavaConverters._

object HtmlParser {

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

  def parseHtml(url: String, html: String): (List[Category], List[LegoPart]) = {
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
    val mainCat = Category(mainNumber, mainName, categoryParents.headOption)
    categories(mainNumber) = mainCat
    categoryParents = mainCat :: categoryParents

    // Extract child categories from div.part_category
    def extractChildCategories(element: Element, parent: Category): Unit = {
      val childDivs = element.children().asScala.filter(e => e.tagName() == "div" && e.classNames().contains("part_category"))
      childDivs.foreach { div =>
        val a = div.selectFirst("a[href^='https://brickarchitect.com/parts/category']")
        if (a != null) {
          getCategoryNumber(a.attr("href")).foreach { number =>
            val name = a.text().trim
            val cat = Category(number, name, Some(parent))
            categories(number) = cat
            extractParts(div, cat)
            extractChildCategories(div, cat)
          }
        }
      }
    }

    // Extract parts from div.parts_results
    def extractParts(element: Element, category: Category): Unit = {
      for {
        results <- element.children().asScala
        if results.tagName() == "div" && results.classNames().contains("parts_results")
        span <- results.select("span.td.part_name").asScala
      } {
        val partNumSpan = span.selectFirst("span.partnum")
        val partNameSpan = span.selectFirst("span.partname")
        val partNumber = if (partNumSpan != null) partNumSpan.text().trim else ""
        val partName = if (partNameSpan != null) partNameSpan.text().trim else ""
        val legoPart = LegoPart(partNumber, partName, getAncestors(category))
        parts = legoPart :: parts
      }
    }

    def getAncestors(cat: Category): List[Category] = {
      val visited = mutable.Set[Category]()
      val result = mutable.ListBuffer[Category]()
      var current: Option[Category] = Some(cat)
      while (current.isDefined && !visited.contains(current.get)) {
        val c = current.get
        visited.add(c)
        result += c
        current = c.parent
      }
      result.toList
    }

    val inlineResults = doc.selectFirst("div.inlineresults")
    extractParts(inlineResults, mainCat)
    extractChildCategories(inlineResults, mainCat)
    (categories.values.toList, parts)
  }
}