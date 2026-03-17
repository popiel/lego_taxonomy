package com.example

object PartNameIndex {
  def buildIndex(parts: List[LegoPart]): Map[String, Set[LegoPart]] = {
    parts.flatMap { part =>
      val words = tokenize(part.name)
      words.map(_ -> part)
    }.groupBy(_._1).view.mapValues(_.map(_._2).toSet).toMap
  }

  def search(index: Map[String, Set[LegoPart]], query: String, limit: Int = 5): List[(LegoPart, Int)] = {
    val queryWords = tokenize(query).toSet
    if (queryWords.isEmpty) return Nil

    val partCounts = scala.collection.mutable.Map[LegoPart, Int]().withDefaultValue(0)

    queryWords.foreach { word =>
      index.get(word).foreach { parts =>
        parts.foreach { part =>
          partCounts(part) = partCounts(part) + 1
        }
      }
    }

    partCounts.toList.sortBy { case (part, count) => (-count, part) }.take(limit).map { case (part, count) => (part, count) }
  }

  def findExactMatch(index: Map[String, Set[LegoPart]], query: String): Option[LegoPart] = {
    val queryWords = tokenize(query).toSet
    if (queryWords.isEmpty) return None

    index.keySet.filter(word => queryWords.contains(word)).foldLeft(Map[LegoPart, Int]().withDefaultValue(0)) { (acc, matchedIndexWord) =>
      index(matchedIndexWord).foldLeft(acc) { (innerAcc, part) =>
        innerAcc + (part -> (innerAcc(part) + 1))
      }
    }.find { case (_, count) => count == queryWords.size }.map(_._1)
  }

  def tokenize(name: String): List[String] = {
    val dimensionPattern = raw"(\d+\s*x\s*\d+(?:\s*x\s*\d+)*)".r
    val step1 = name.replace("\u00D7", "x").toLowerCase
    val step2 = dimensionPattern.replaceAllIn(step1, m => m.matched.replaceAll("\\s+", ""))

    val words = step2.split("\\s+").filter(_.nonEmpty).flatMap { word =>
      val noComma = word.stripSuffix(",")

      val letterBeforePeriod = raw"([a-zA-Z]+)\.(.+)".r
      noComma match {
        case letterBeforePeriod(before, after) =>
          List(s"$before.", after.toLowerCase)
        case _ =>
          List(noComma.toLowerCase)
      }
    }.filter(_.nonEmpty).toList

    words
  }
}
