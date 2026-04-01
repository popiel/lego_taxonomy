package com.wolfskeep

import java.io.{FileInputStream, InputStreamReader, Reader}
import scala.io.Source
import scala.util.Try

class CsvReader {
  def readColoredParts(
    filePath: String,
    colorIdToName: Map[Int, String] = Map.empty,
    elementIdToPartColor: Map[Long, (String, String, String)] = Map.empty
  ): List[ColoredPart] = {
    readColoredPartsFromSource(Source.fromFile(filePath), colorIdToName, elementIdToPartColor)
  }

  def readColoredPartsFromString(
    content: String,
    colorIdToName: Map[Int, String] = Map.empty,
    elementIdToPartColor: Map[Long, (String, String, String)] = Map.empty
  ): List[ColoredPart] = {
    readColoredPartsFromSource(Source.fromString(content), colorIdToName, elementIdToPartColor)
  }

  def readColoredPartsFromReader(
    reader: Reader,
    colorIdToName: Map[Int, String] = Map.empty,
    elementIdToPartColor: Map[Long, (String, String, String)] = Map.empty
  ): List[ColoredPart] = {
    val bufferedReader = new java.io.BufferedReader(reader)
    val lines = scala.collection.Iterator.continually(bufferedReader.readLine()).takeWhile(_ != null).toList
    parseLines(lines, colorIdToName, elementIdToPartColor)
  }

  private def readColoredPartsFromSource(
    source: Source,
    colorIdToName: Map[Int, String] = Map.empty,
    elementIdToPartColor: Map[Long, (String, String, String)] = Map.empty
  ): List[ColoredPart] = {
    val lines = source.getLines().toList
    parseLines(lines, colorIdToName, elementIdToPartColor)
  }

  private def parseLines(
    lines: List[String],
    colorIdToName: Map[Int, String] = Map.empty,
    elementIdToPartColor: Map[Long, (String, String, String)] = Map.empty
  ): List[ColoredPart] = {
    if (lines.isEmpty) return Nil

    val headers = parseCsvLine(lines.head).map(_.trim)
    val partNumberIndex = findColumnIndex(headers, Seq("Part", "partNumber", "DesignID", "BLItemNo"))
    val colorIndex = findColumnIndex(headers, Seq("Color", "color", "Colour", "ColorName"))
    val quantityIndex = findColumnIndex(headers, Seq("Quantity", "quantity", "Qty"))
    val nameIndex = findColumnIndex(headers, Seq("name", "ElementName", "PartName"))
    val elementIdIndex = findColumnIndex(headers, Seq("ElementId", "ElementID", "element_id", "elementId"))
    val isSpareIndex = findColumnIndex(headers, Seq("Is Spare"))

    val isElementOnlyMode = partNumberIndex.isEmpty && colorIndex.isEmpty && 
      elementIdIndex.isDefined && elementIdToPartColor.nonEmpty

    if (isElementOnlyMode) {
      parseElementOnlyLines(lines.tail, elementIdIndex.get, quantityIndex, elementIdToPartColor)
    } else {
      parseStandardLines(lines.tail, partNumberIndex, colorIndex, quantityIndex, nameIndex, 
        elementIdIndex, isSpareIndex, colorIdToName)
    }
  }

  private def parseElementOnlyLines(
    lines: List[String],
    elementIdIndex: Int,
    quantityIndex: Option[Int],
    elementIdToPartColor: Map[Long, (String, String, String)]
  ): List[ColoredPart] = {
    lines.flatMap { line =>
      val fields = parseCsvLine(line)
      for {
        elementIdStr <- Try(fields(elementIdIndex).trim).toOption.filter(_.nonEmpty)
        elementId <- Try(elementIdStr.toLong).toOption
        quantity <- quantityIndex.flatMap(i => Try(fields(i).trim.toInt).toOption)
      } yield {
        val (partNumber, colorName, partName) = elementIdToPartColor.get(elementId)
          .getOrElse((s"unknown-element-$elementId", "unknown-0", ""))
        ColoredPart(partNumber, colorName, quantity, partName, Some(elementId.toString))
      }
    }
  }

  private def parseStandardLines(
    lines: List[String],
    partNumberIndex: Option[Int],
    colorIndex: Option[Int],
    quantityIndex: Option[Int],
    nameIndex: Option[Int],
    elementIdIndex: Option[Int],
    isSpareIndex: Option[Int],
    colorIdToName: Map[Int, String]
  ): List[ColoredPart] = {
    lines.flatMap { line =>
      val fields = parseCsvLine(line)
      val isSpare = isSpareIndex.flatMap(i => Try(fields(i).trim).toOption).exists(_.equalsIgnoreCase("TRUE"))
      if (isSpare) None
      else {
        for {
          partNumber <- partNumberIndex.flatMap(i => Try(fields(i).trim).toOption).filter(_.nonEmpty)
          rawColor <- colorIndex.flatMap(i => Try(fields(i).trim).toOption).filter(_.nonEmpty)
          quantity <- quantityIndex.flatMap(i => Try(fields(i).trim.toInt).toOption)
        } yield {
          val name = nameIndex.flatMap(i => Try(fields(i).trim).toOption).filter(_.nonEmpty).getOrElse("")
          val elementId = elementIdIndex.flatMap(i => Try(fields(i).trim).toOption).filter(_.nonEmpty)
          val color = rawColor.toIntOption match {
            case Some(colorId) => colorIdToName.getOrElse(colorId, s"unknown-$colorId")
            case None => rawColor
          }
          ColoredPart(partNumber, color, quantity, name, elementId)
        }
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