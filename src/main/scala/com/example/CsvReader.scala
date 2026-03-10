package com.example

import scala.io.Source
import scala.util.Try

class CsvReader {
  def readColoredParts(filePath: String): List[ColoredPart] = {
    val lines = Source.fromFile(filePath).getLines().toList
    if (lines.isEmpty) return Nil

    val headers = parseCsvLine(lines.head).map(_.trim)
    val partNumberIndex = findColumnIndex(headers, Seq("partNumber", "DesignID", "BLItemNo"))
    val colorIndex = findColumnIndex(headers, Seq("color", "Colour", "ColorName"))
    val quantityIndex = findColumnIndex(headers, Seq("quantity", "Qty"))
    val nameIndex = findColumnIndex(headers, Seq("name", "ElementName", "PartName"))

    lines.tail.flatMap { line =>
      val fields = parseCsvLine(line)
      for {
        partNumber <- partNumberIndex.flatMap(i => Try(fields(i).trim).toOption).filter(_.nonEmpty)
        color <- colorIndex.flatMap(i => Try(fields(i).trim).toOption).filter(_.nonEmpty)
        quantity <- quantityIndex.flatMap(i => Try(fields(i).trim.toInt).toOption)
      } yield {
        val name = nameIndex.flatMap(i => Try(fields(i).trim).toOption).filter(_.nonEmpty).getOrElse("")
        ColoredPart(partNumber, color, quantity, name)
      }
    }
  }

  private def findColumnIndex(headers: List[String], possibleNames: Seq[String]): Option[Int] = {
    possibleNames.flatMap(name => headers.zipWithIndex.find(_._1.equalsIgnoreCase(name)).map(_._2)).headOption
  }

  private def parseCsvLine(line: String): List[String] = {
    val sb = new StringBuilder
    var fields = List.empty[String]
    var inQuotes = false
    var i = 0
    while (i < line.length) {
      val c = line(i)
      if (c == '"') {
        if (inQuotes && i + 1 < line.length && line(i + 1) == '"') {
          sb.append('"')
          i += 1
        } else {
          inQuotes = !inQuotes
        }
      } else if (c == ',' && !inQuotes) {
        fields = sb.toString :: fields
        sb.clear()
      } else if (c == '\\' && i + 1 < line.length) {
        val next = line(i + 1)
        next match {
          case '\\' => sb.append('\\')
          case '"' => sb.append('"')
          case ',' => sb.append(',')
          case _ => sb.append('\\').append(next)
        }
        i += 1
      } else {
        sb.append(c)
      }
      i += 1
    }
    fields = sb.toString :: fields
    fields.reverse
  }
}