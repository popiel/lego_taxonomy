package com.wolfskeep

import java.io.{BufferedReader, InputStream, InputStreamReader, Reader, StringReader}
import java.util.zip.ZipFile
import java.util.zip.ZipInputStream
import scala.collection.mutable
import scala.io.Source
import scala.util.Try

class StudioIoReader {
  
  // LDraw colors - kept for historical reference, no longer used
  // private val ldrawColorMap: Map[Int, String] = Map(
  //   0 -> "Black",
  //   1 -> "Blue",
  //   ...
  // )
  
  // BrickLink colors - used by Studio .io files
  private val colorMap: Map[Int, String] = Map(
    // Primary colors
    1 -> "White",
    11 -> "Black",
    5 -> "Red",
    7 -> "Blue",
    
    // Grays
    86 -> "Light Bluish Gray",
    85 -> "Dark Bluish Gray",
    9 -> "Light Gray",
    10 -> "Dark Gray",
    49 -> "Very Light Gray",
    99 -> "Very Light Bluish Gray",
    
    // Transparent colors
    12 -> "Trans-Clear",
    13 -> "Trans-Black",
    14 -> "Trans-Dark Blue",
    15 -> "Trans-Green",
    16 -> "Trans-Dark Green",
    17 -> "Trans-Red",
    18 -> "Trans-Yellow",
    19 -> "Trans-Neon Green",
    20 -> "Trans-Bright Green",
    21 -> "Trans-Light Blue",
    22 -> "Trans-Medium Blue",
    23 -> "Trans-Light Green",
    24 -> "Trans-Reddish Lilac",
    25 -> "Trans-Light Purple",
    26 -> "Trans-Dark Pink",
    27 -> "Trans-Light Pink",
    28 -> "Trans-Yellowish Green",
    29 -> "Trans-Light Yellow",
    30 -> "Trans-Neon Yellow",
    31 -> "Trans-Orange",
    32 -> "Trans-Very Light Blue",
    33 -> "Trans-Neon Red",
    40 -> "Trans-Brown",
    41 -> "Trans-Medium Blue",
    42 -> "Trans-Red",
    43 -> "Trans-Purple",
    44 -> "Trans-Yellow",
    45 -> "Trans-Pink",
    46 -> "Trans-Clear",
    47 -> "Trans-Black",
    52 -> "Trans-Purple",
    54 -> "Trans-Neon Yellow",
    57 -> "Trans-Orange",
    
    // Additional solid colors
    3 -> "Yellow",
    4 -> "Orange",
    6 -> "Green",
    8 -> "Brown",
    2 -> "Tan",
    59 -> "Dark Red",
    63 -> "Dark Blue",
    37 -> "Medium Green",
    36 -> "Bright Green",
    38 -> "Light Green",
    40 -> "Light Turquoise",
    39 -> "Dark Turquoise",
    41 -> "Aqua",
    42 -> "Medium Blue",
    62 -> "Light Blue",
    87 -> "Sky Blue",
    72 -> "Maersk Blue",
    43 -> "Violet",
    24 -> "Purple",
    71 -> "Magenta",
    23 -> "Pink",
    47 -> "Dark Pink",
    104 -> "Bright Pink",
    26 -> "Light Salmon",
    25 -> "Salmon",
    28 -> "Nougat",
    90 -> "Light Nougat",
    150 -> "Medium Nougat",
    88 -> "Reddish Brown",
    120 -> "Dark Brown",
    68 -> "Dark Orange",
    31 -> "Medium Orange",
    32 -> "Light Orange",
    110 -> "Bright Light Orange",
    33 -> "Light Yellow",
    35 -> "Light Lime",
    34 -> "Lime",
    76 -> "Medium Lime",
    155 -> "Olive Green",
    80 -> "Dark Green",
    48 -> "Sand Green",
    55 -> "Sand Blue",
    54 -> "Sand Purple",
    58 -> "Sand Red",
    69 -> "Dark Tan",
    91 -> "Light Brown",
    106 -> "Fabuland Brown",
    73 -> "Medium Violet",
    44 -> "Light Violet",
    89 -> "Dark Purple",
    157 -> "Medium Lavender",
    154 -> "Lavender",
    94 -> "Medium Dark Pink",
    56 -> "Rose Pink",
    175 -> "Warm Pink",
    220 -> "Coral",
    96 -> "Very Light Orange",
    167 -> "Reddish Orange",
    231 -> "Dark Salmon",
    172 -> "Warm Yellowish Orange",
    161 -> "Dark Yellow",
    173 -> "Ochre Yellow",
    27 -> "Rust",
    165 -> "Neon Orange",
    166 -> "Neon Green",
    236 -> "Neon Yellow",
    171 -> "Lemon",
    158 -> "Yellowish Green",
    247 -> "Little Robots Blue",
    153 -> "Dark Azure",
    105 -> "Bright Light Blue",
    152 -> "Light Aqua",
    174 -> "Blue Violet",
    97 -> "Royal Blue",
    245 -> "Lilac",
    246 -> "Light Lilac",
    93 -> "Light Purple",
    227 -> "Clikits Lavender",
    240 -> "Medium Brown",
    241 -> "Medium Tan",
    225 -> "Dark Nougat",
    160 -> "Fabuland Orange",
    29 -> "Earth Orange",
    168 -> "Umber",
    169 -> "Sienna",
    103 -> "Bright Light Yellow",
    248 -> "Fabuland Lime"
  )

  private case class ParsedSubfile(
    name: String,
    directParts: List[(String, Int, Int)],  // (partNumber, colorIndex, quantity)
    subfileRefs: List[String]                  // referenced subfile names
  )

  def readColoredParts(filePath: String): List[ColoredPart] = {
    val zipFile = new ZipFile(filePath)
    try {
      val entry = zipFile.getEntry("model2.ldr")
      if (entry == null) {
        throw new RuntimeException("model2.ldr not found in .io file")
      }
      val reader = new InputStreamReader(zipFile.getInputStream(entry))
      try {
        readColoredPartsFromReader(reader)
      } finally {
        reader.close()
      }
    } finally {
      zipFile.close()
    }
  }

  def readColoredParts(inputStream: InputStream): List[ColoredPart] = {
    val zipStream = new ZipInputStream(inputStream)
    var reader: Reader = null
    try {
      var entry = zipStream.getNextEntry()
      while (entry != null && reader == null) {
        if (entry.getName == "model2.ldr") {
          reader = new InputStreamReader(zipStream)
        } else {
          entry = zipStream.getNextEntry()
        }
      }
      if (reader == null) {
        throw new RuntimeException("model2.ldr not found in .io file")
      }
      readColoredPartsFromReader(reader)
    } finally {
      if (reader != null) reader.close()
    }
  }

  def readColoredPartsFromReader(reader: Reader): List[ColoredPart] = {
    val (firstSubfileName, subfiles, partDescriptions) = parseLDraw(reader)
    val partCounts = mutable.Map[(String, String), Int]()

    val mainSubfile = firstSubfileName match {
      case null => subfiles.values.headOption.getOrElse(null)
      case name => subfiles.get(name.toLowerCase).orElse(subfiles.values.headOption).getOrElse(null)
    }
    countParts(subfiles, mainSubfile, Set.empty, 1, partCounts)

    partCounts.map { case ((partNumber, color), qty) =>
      val colorName = colorMap.getOrElse(color.toInt, s"Color$color")
      val partName = partDescriptions.getOrElse(partNumber, "")
      ColoredPart(partNumber, colorName, qty, partName)
    }.toList.sortBy(p => (p.partNumber, p.color))
  }

  def readColoredPartsFromString(content: String): List[ColoredPart] = {
    val reader = new StringReader(content)
    readColoredPartsFromReader(reader)
  }

  private def parseLDraw(reader: Reader): (String, Map[String, ParsedSubfile], Map[String, String]) = {
    val bufferedReader = new BufferedReader(reader)
    val subfilesMap = mutable.Map[String, ParsedSubfile]()
    val partDescriptions = mutable.Map[String, String]()

    var firstSubfileName: String = null
    var currentSubfileName: String = null
    var currentDirectParts = List.empty[(String, Int, Int)]
    var currentSubfileRefs = List.empty[String]
    var hasSubfiles = false
    var lineNum = 0
    var inDatSubfile = false

    var line = bufferedReader.readLine()
    while (line != null) {
      lineNum += 1
      val trimmed = line.trim
      val lineToCheck = trimmed.stripPrefix("\uFEFF")

      if (lineToCheck.startsWith("0 FILE ")) {
        // Save previous subfile if exists
        if (currentSubfileName != null) {
          subfilesMap(currentSubfileName.toLowerCase) =
            ParsedSubfile(currentSubfileName, currentDirectParts.reverse, currentSubfileRefs.reverse)
        }

        // Start new subfile
        currentSubfileName = lineToCheck.replaceFirst("0 FILE ", "").trim
        currentDirectParts = Nil
        currentSubfileRefs = Nil
        hasSubfiles = true
        inDatSubfile = currentSubfileName.endsWith(".dat") || currentSubfileName.endsWith(".DAT")
        lineNum = 1  // Reset line number for new subfile

        if (firstSubfileName == null) {
          firstSubfileName = currentSubfileName
        }

      } else if (lineToCheck == "0 NOFILE") {
        // Save current subfile
        if (currentSubfileName != null) {
          subfilesMap(currentSubfileName.toLowerCase) =
            ParsedSubfile(currentSubfileName, currentDirectParts.reverse, currentSubfileRefs.reverse)
          currentSubfileName = null
          currentDirectParts = Nil
          currentSubfileRefs = Nil
        }

      } else if (inDatSubfile && lineNum == 2 && lineToCheck.startsWith("0 ") && !lineToCheck.startsWith("0 Name:")) {
        // Part description line for .dat subfiles
        val description = lineToCheck.stripPrefix("0 ")
        val partNumber = extractPartNumber(currentSubfileName)
        partDescriptions(partNumber) = description
        inDatSubfile = false

      } else if (lineToCheck.startsWith("1 ") || lineToCheck.startsWith("11 ")) {
        // Part line
        parseLdrLine(lineToCheck).foreach { case (partNumber, colorIndex, qty) =>
          if (partNumber.endsWith(".dat") || partNumber.endsWith(".DAT")) {
            currentDirectParts ::= (extractPartNumber(partNumber), colorIndex, qty)
          } else {
            currentSubfileRefs ::= partNumber
          }
        }
      }

      line = bufferedReader.readLine()
    }

    // Save last subfile if not already saved (when file doesn't end with 0 NOFILE)
    if (currentSubfileName != null && !subfilesMap.contains(currentSubfileName.toLowerCase)) {
      subfilesMap(currentSubfileName.toLowerCase) =
        ParsedSubfile(currentSubfileName, currentDirectParts.reverse, currentSubfileRefs.reverse)
    }

    // Handle case with no subfile structure (treat entire content as main)
    if (!hasSubfiles && firstSubfileName == null) {
      subfilesMap("main") = ParsedSubfile("main", currentDirectParts.reverse, currentSubfileRefs.reverse)
      firstSubfileName = "main"
    }

    (firstSubfileName, subfilesMap.toMap, partDescriptions.toMap)
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
