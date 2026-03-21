package com.wolfskeep

case class TaxonomyData(
  categories: Set[Category],
  parts: List[LegoPart]
) {
  lazy val partNumberOrAltToPart: Map[String, LegoPart] = 
    parts.flatMap(part => (part.partNumber :: part.altNumbers.toList).map(_ -> part)).toMap
  
  lazy val wordIndex: Map[String, Set[LegoPart]] = 
    parts.flatMap { part =>
      TaxonomyData.tokenize(part.name).map(_ -> part)
    }.groupBy(_._1).view.mapValues(_.map(_._2).toSet).toMap
  
  def findPart(partNumberOrAlt: String): Option[LegoPart] = 
    partNumberOrAltToPart.get(partNumberOrAlt)
  
  def searchByName(query: String, limit: Int = 5): List[(LegoPart, Int)] = {
    val queryWords = TaxonomyData.tokenize(query).toSet
    if (queryWords.isEmpty) return Nil
    val partCounts = scala.collection.mutable.Map[LegoPart, Int]().withDefaultValue(0)
    queryWords.foreach { word =>
      wordIndex.get(word).foreach { parts =>
        parts.foreach { part => partCounts(part) = partCounts(part) + 1 }
      }
    }
    partCounts.toList.sortBy { case (part, count) => (-count, part) }.take(limit)
  }
  
  def findExactMatch(query: String): Option[LegoPart] = {
    val queryWords = TaxonomyData.tokenize(query).toSet
    if (queryWords.isEmpty) return None
    wordIndex.keySet.filter(word => queryWords.contains(word))
      .foldLeft(Map[LegoPart, Int]().withDefaultValue(0)) { (acc, matchedIndexWord) =>
        wordIndex(matchedIndexWord).foldLeft(acc) { (innerAcc, part) =>
          innerAcc + (part -> (innerAcc(part) + 1))
        }
      }.find { case (_, count) => count == queryWords.size }.map(_._1)
  }
  
  def findCommonCategoryPrefix(parts: List[LegoPart]): List[Category] = {
    if (parts.isEmpty) return Nil
    val hierarchies = parts.map(_.categories)
    if (hierarchies.exists(_.isEmpty)) return Nil
    val minLength = hierarchies.map(_.length).min
    if (minLength == 0) return Nil
    val commonPrefix = (0 until minLength).collect { i =>
      val categoryAtPosition = hierarchies.map(_.apply(i))
      if (categoryAtPosition.forall(_ == categoryAtPosition.head)) Some(categoryAtPosition.head) else None
    }.takeWhile(_.isDefined).flatten
    commonPrefix.toList
  }
}

object TaxonomyData {
  private val dimensionPattern = raw"(\d+\s*x\s*\d+(?:\s*x\s*\d+)*)".r
  private val letterBeforePeriod = raw"([a-zA-Z]+)\.(.+)".r
  private val numberDashAlpha = raw"(\d+)-([a-zA-Z]+)".r

  def tokenize(name: String): List[String] = {
    val step1 = name.replace("\u00D7", "x").toLowerCase
    val step2 = dimensionPattern.replaceAllIn(step1, m => m.matched.replaceAll("\\s+", ""))
    val words = step2.split("\\s+").filter(_.nonEmpty).flatMap { word =>
      val noComma = word.stripSuffix(",")
      val splitWord = noComma match {
        case letterBeforePeriod(before, after) => List(s"$before.", after.toLowerCase)
        case _ => List(noComma.toLowerCase)
      }
      splitWord.flatMap { w =>
        w match {
          case numberDashAlpha(num, alpha) => List(num, alpha)
          case _ => List(w)
        }
      }
    }.filter(_.nonEmpty).toList
    words
  }
}