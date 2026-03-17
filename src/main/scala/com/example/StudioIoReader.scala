package com.example

import java.util.zip.ZipFile
import scala.collection.mutable
import scala.io.Source
import scala.util.Try

class StudioIoReader {
  
  private val colorMap: Map[Int, String] = Map(
    0 -> "Black",
    1 -> "Blue",
    2 -> "Green",
    3 -> "Cyan",
    4 -> "Red",
    5 -> "Red",
    6 -> "Yellow",
    7 -> "Blue",
    8 -> "Gray",
    9 -> "Dark Gray",
    10 -> "Dark Blue",
    11 -> "Dark Green",
    12 -> "Light Blue",
    13 -> "Aquamarine",
    14 -> "Orange",
    15 -> "Yellow",
    16 -> "Red",
    17 -> "Dark Pink",
    18 -> "Pink",
    19 -> "Light Gray",
    20 -> "Dark Gray",
    21 -> "Blue",
    22 -> "Light Blue",
    23 -> "Black",
    24 -> "Translucent Blue",
    25 -> "Translucent Green",
    26 -> "Translucent Red",
    27 -> "Translucent Yellow",
    28 -> "Translucent White",
    29 -> "Gold",
    30 -> "Bright Green",
    31 -> "Bright Blue",
    32 -> "Bright Red",
    33 -> "Yellow",
    34 -> "White",
    35 -> "Black",
    36 -> "Lemon",
    37 -> "Magenta",
    38 -> "Translucent Pink",
    39 -> "Translucent Black"
  )

  private case class ParsedSubfile(
    name: String,
    directParts: List[(String, Int, Int)],  // (partNumber, colorIndex, quantity)
    subfileRefs: List[String]                  // referenced subfile names
  )

  def readColoredParts(filePath: String): List[ColoredPart] = {
    val zipFile = new ZipFile(filePath)
    try {
      val entry = zipFile.getEntry("model.ldr")
      if (entry == null) {
        throw new RuntimeException("model.ldr not found in .io file")
      }
      val content = Source.fromInputStream(zipFile.getInputStream(entry)).mkString
      readColoredPartsFromString(content)
    } finally {
      zipFile.close()
    }
  }

  def readColoredPartsFromString(content: String): List[ColoredPart] = {
    val subfiles = parseAllSubfiles(content)
    val partCounts = mutable.Map[(String, String), Int]()
    
    val mainSubfile = subfiles.get("simple.io").orElse(subfiles.get("main")).orElse(subfiles.values.headOption).getOrElse(null)
    countParts(subfiles, mainSubfile, Set.empty, 1, partCounts)

    partCounts.map { case ((partNumber, color), qty) =>
      val colorName = colorMap.getOrElse(color.toInt, s"Color$color")
      ColoredPart(partNumber, colorName, qty)
    }.toList.sortBy(p => (p.partNumber, p.color))
  }

  private def parseAllSubfiles(content: String): Map[String, ParsedSubfile] = {
    val subfilesMap = mutable.Map[String, ParsedSubfile]()
    
    // Split content by subfiles (0 FILE ... 0 NOFILE)
    val subfileSections = content.split("0 NOFILE")
    var hasSubfiles = false
    
    for (section <- subfileSections) {
      val trimmed = section.trim
      val cleanContent = if (trimmed.startsWith("\uFEFF")) trimmed.drop(1) else trimmed
      if (cleanContent.startsWith("0 FILE")) {
        hasSubfiles = true
        // Extract subfile name from "0 FILE filename"
        val nameLine = trimmed.linesIterator.next().trim
        val subfileName = nameLine.replaceFirst("0 FILE", "").trim
        
        if (subfileName.nonEmpty) {
          val parsed = parseSubfile(subfileName, trimmed)
          subfilesMap(subfileName.toLowerCase) = parsed
        }
      }
    }
    
    // If no subfile structure found, treat entire content as main subfile
    if (!hasSubfiles && content.trim.nonEmpty) {
      val parsed = parseSubfile("main", content)
      subfilesMap("main") = parsed
    }
    
    subfilesMap.toMap
  }

  private def parseSubfile(name: String, content: String): ParsedSubfile = {
    var directParts = List.empty[(String, Int, Int)]
    var subfileRefs = List.empty[String]
    
    for (line <- content.linesIterator) {
      val trimmed = line.trim
      if (trimmed.startsWith("1 ") || trimmed.startsWith("11 ")) {
        parseLdrLine(trimmed) match {
          case Some((partNumber, colorIndex, qty)) =>
            if (partNumber.endsWith(".dat") || partNumber.endsWith(".DAT")) {
              directParts = (extractPartNumber(partNumber), colorIndex, qty) :: directParts
            } else {
              subfileRefs = partNumber :: subfileRefs
            }
          case None =>
        }
      }
    }
    
    ParsedSubfile(name, directParts, subfileRefs)
  }

  private def countParts(
    subfiles: Map[String, ParsedSubfile],
    subfile: ParsedSubfile,
    visited: Set[String],
    multiplier: Int,
    partCounts: mutable.Map[(String, String), Int]
  ): Unit = {
    if (subfile == null || visited.contains(subfile.name.toLowerCase)) {
      return
    }
    
    val newVisited = visited + subfile.name.toLowerCase
    
    // Count direct parts with multiplier
    for ((partNumber, colorIndex, qty) <- subfile.directParts) {
      val key = (partNumber, colorIndex.toString)
      partCounts(key) = partCounts.getOrElse(key, 0) + multiplier * qty
    }
    
    // Count how many times each subfile is referenced
    val refCounts = subfile.subfileRefs.groupBy(_.toLowerCase).view.mapValues(_.size)
    
    // Process each subfile ref with the accumulated multiplier
    for ((refLower, count) <- refCounts) {
      subfiles.get(refLower) match {
        case Some(nestedSubfile) =>
          countParts(subfiles, nestedSubfile, newVisited, multiplier * count, partCounts)
        case None =>
      }
    }
  }

  private def parseLdrLine(line: String): Option[(String, Int, Int)] = {
    val trimmed = line.trim
    val parts = trimmed.split("\\s+").filter(_.nonEmpty)
    if (parts.length < 15) return None

    Try {
      val lineType = parts(0).toInt
      val colorIndex = parts(1).toInt
      
      val fileNameIndex = if (lineType == 11) 15 else 14
      if (parts.length <= fileNameIndex) return None
      
      val fileName = parts(fileNameIndex)
      
      val quantity = if (lineType == 11 && parts.length > 2) {
        parts(2).toInt
      } else {
        1
      }
      Some((fileName, colorIndex, quantity))
    }.getOrElse(None)
  }

  private def extractPartNumber(fileName: String): String = {
    val name = fileName.replaceAll("\\\\", "/").split("/").last
    name.replaceAll("\\.dat$", "").replaceAll("\\.DAT$", "")
  }
}
