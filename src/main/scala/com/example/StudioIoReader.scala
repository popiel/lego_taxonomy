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
    3 -> "Dark Turquoise",
    4 -> "Red",
    5 -> "Dark Pink",
    6 -> "Brown",
    7 -> "Light Grey",
    8 -> "Dark Grey",
    9 -> "Light Blue",
    10 -> "Bright Green",
    11 -> "Light Turquoise",
    12 -> "Salmon",
    13 -> "Pink",
    14 -> "Yellow",
    15 -> "White",
    17 -> "Light Green",
    18 -> "Light Yellow",
    19 -> "Tan",
    20 -> "Light Violet",
    21 -> "Glow In Dark Opaque",
    22 -> "Purple",
    23 -> "Dark Blue Violet",
    24 -> "Translucent Brown",
    25 -> "Orange",
    26 -> "Magenta",
    27 -> "Lime",
    28 -> "Dark Tan",
    29 -> "Bright Pink",
    30 -> "Medium Lavender",
    31 -> "Lavender",
    32 -> "Trans Black IR Lens",
    33 -> "Trans Dark Blue",
    34 -> "Trans Green",
    35 -> "Trans Bright Green",
    36 -> "Trans Red",
    37 -> "Trans Dark Pink",
    38 -> "Trans Neon Orange",
    39 -> "Trans Very Light Blue",
    40 -> "Trans Brown",
    41 -> "Trans Medium Blue",
    42 -> "Trans Neon Green",
    43 -> "Trans Light Blue",
    44 -> "Trans Bright Reddish Lilac",
    45 -> "Trans Pink",
    46 -> "Trans Yellow",
    47 -> "Trans Clear",
    52 -> "Trans Purple",
    54 -> "Trans Neon Yellow",
    57 -> "Trans Orange",
    60 -> "Chrome Antique Brass",
    61 -> "Chrome Blue",
    62 -> "Chrome Green",
    63 -> "Chrome Pink",
    64 -> "Chrome Black",
    65 -> "Rubber Yellow",
    66 -> "Rubber Black",
    67 -> "Rubber Blue",
    68 -> "Very Light Orange",
    69 -> "Bright Reddish Lilac",
    70 -> "Reddish Brown",
    71 -> "Light Bluish Grey",
    72 -> "Dark Bluish Grey",
    73 -> "Medium Blue",
    74 -> "Medium Green",
    75 -> "Speckle Black Copper",
    76 -> "Speckle Dark Bluish Grey Silver",
    77 -> "Light Pink",
    78 -> "Light Nougat",
    79 -> "Milky White",
    80 -> "Metallic Silver",
    81 -> "Metallic Green",
    82 -> "Metallic Gold",
    83 -> "Pearl Black",
    84 -> "Medium Nougat",
    85 -> "Medium Lilac",
    86 -> "Light Brown",
    87 -> "Metallic Dark Grey",
    89 -> "Blue Violet",
    92 -> "Nougat",
    100 -> "Light Salmon",
    110 -> "Violet",
    112 -> "Medium Violet",
    115 -> "Medium Lime",
    118 -> "Aqua",
    120 -> "Light Lime",
    121 -> "Light Orange",
    123 -> "Dark Salmon",
    125 -> "Spud Orange",
    128 -> "Dark Nougat",
    132 -> "Speckle Black Silver",
    133 -> "Speckle Black Gold",
    134 -> "Copper",
    135 -> "Pearl Light Grey",
    137 -> "Metallic Blue",
    142 -> "Pearl Light Gold",
    147 -> "Pearl Dark Gold",
    148 -> "Pearl Dark Grey",
    150 -> "Pearl Very Light Grey",
    151 -> "Very Light Bluish Grey",
    158 -> "Trans Neon Red",
    176 -> "Pearl Red",
    178 -> "Pearl Yellow",
    179 -> "Pearl Silver",
    180 -> "Dark Yellow",
    183 -> "Pearl White",
    184 -> "Metallic Bright Red",
    185 -> "Metallic Bright Blue",
    186 -> "Metallic Dark Green",
    187 -> "Pearl Brown",
    189 -> "Reddish Gold",
    191 -> "Bright Light Orange",
    196 -> "Chrome Gold",
    200 -> "Lemon Metallic",
    212 -> "Bright Light Blue",
    213 -> "Medium Blue Violet",
    216 -> "Rust",
    218 -> "Reddish Lilac",
    219 -> "Lilac",
    220 -> "Light Lilac",
    225 -> "Warm Yellowish Orange",
    226 -> "Bright Light Yellow",
    227 -> "Trans Bright Light Green",
    231 -> "Trans Bright Light Orange",
    232 -> "Sky Blue",
    234 -> "Trans Fire Yellow",
    256 -> "Rubber Black",
    272 -> "Dark Blue",
    273 -> "Rubber Blue",
    284 -> "Trans Reddish Lilac",
    285 -> "Trans Light Green",
    288 -> "Dark Green",
    293 -> "Trans Light Blue Violet",
    295 -> "Flamingo Pink",
    296 -> "Cool Silver",
    297 -> "Pearl Gold",
    308 -> "Dark Brown",
    313 -> "Maersk Blue",
    315 -> "Flat Silver",
    316 -> "Titanium Metallic",
    320 -> "Dark Red",
    321 -> "Dark Azure",
    322 -> "Medium Azure",
    323 -> "Light Aqua",
    324 -> "Rubber Red",
    326 -> "Yellowish Green",
    329 -> "Glow In Dark White",
    330 -> "Olive Green",
    332 -> "Fluorescent Red Ink",
    333 -> "Fluorescent Green Ink",
    335 -> "Sand Red",
    339 -> "Glitter Trans Neon Green",
    341 -> "Glitter Trans Orange",
    342 -> "Conductive Black",
    346 -> "Reddish Copper",
    350 -> "Rubber Orange",
    351 -> "Medium Dark Pink",
    353 -> "Coral",
    360 -> "Opal Trans Clear",
    362 -> "Opal Trans Light Blue",
    363 -> "Opal Trans Black",
    364 -> "Opal Trans Dark Pink",
    365 -> "Opal Trans Purple",
    366 -> "Earth Orange",
    367 -> "Opal Trans Green",
    368 -> "Neon Yellow",
    370 -> "Medium Brown",
    371 -> "Medium Tan",
    373 -> "Sand Purple",
    375 -> "Rubber Light Grey",
    376 -> "Opal Trans Yellow",
    378 -> "Sand Green",
    379 -> "Sand Blue",
    383 -> "Chrome Silver",
    402 -> "Reddish Orange",
    406 -> "Rubber Dark Blue",
    422 -> "Sienna Brown",
    423 -> "Umber Brown",
    424 -> "Ochre Yellow",
    430 -> "Warm Pink",
    431 -> "Bright Blue Violet",
    449 -> "Rubber Purple",
    450 -> "Fabuland Brown",
    462 -> "Medium Orange",
    484 -> "Dark Orange",
    490 -> "Rubber Lime",
    496 -> "Rubber Light Bluish Grey",
    503 -> "Very Light Grey",
    504 -> "Rubber Flat Silver",
    508 -> "Fabuland Red",
    509 -> "Fabuland Orange",
    510 -> "Fabuland Lime",
    511 -> "Rubber White",
    10002 -> "Rubber Green",
    10003 -> "Rubber Dark Turquoise",
    10005 -> "Rubber Dark Pink",
    10008 -> "Rubber Dark Grey",
    10010 -> "Rubber Bright Green",
    10015 -> "Lemon",
    10017 -> "Rose Pink",
    10019 -> "Rubber Tan",
    10022 -> "Yellowish Dark Pink",
    10026 -> "Rubber Magenta",
    10028 -> "Rubber Dark Tan",
    10029 -> "Rubber Bright Pink",
    10030 -> "Rubber Medium Lavender",
    10031 -> "Rubber Lavender",
    10035 -> "Rubber Pearl Light Grey",
    10037 -> "Rubber Metallic Blue",
    10045 -> "Metallic Light Blue",
    10046 -> "Metallic Pink",
    10049 -> "Rubber Metallic Light Pink",
    10070 -> "Rubber Reddish Brown",
    10072 -> "Rubber Dark Bluish Grey",
    10073 -> "Rubber Medium Blue",
    10078 -> "Rubber Light Nougat",
    10084 -> "Rubber Medium Nougat",
    10085 -> "Rubber Medium Lilac",
    10092 -> "Rubber Nougat",
    10135 -> "Rubber Pearl Light Grey",
    10137 -> "Rubber Metallic Blue",
    10147 -> "Rubber Pearl Dark Gold",
    10148 -> "Rubber Pearl Dark Grey",
    10150 -> "Rubber Pearl Very Light Grey",
    10151 -> "Rubber Very Light Bluish Grey",
    10375 -> "Trans Black",
    11015 -> "Trans White",
    11019 -> "Trans Tan",
    30000 -> "Modulex Clear",
    30001 -> "Modulex White",
    30002 -> "Modulex Light Grey",
    30003 -> "Modulex Black",
    30004 -> "Modulex Terracotta",
    30005 -> "Modulex Buff",
    30006 -> "Modulex Ochre Yellow",
    30007 -> "Modulex Olive Green",
    30008 -> "Modulex Teal Blue",
    30009 -> "Modulex Brown",
    30010 -> "Modulex Strong Red",
    30011 -> "Modulex Pastel Blue",
    30012 -> "Modulex Orange",
    30013 -> "Modulex Red",
    30014 -> "Modulex Pastel Green",
    30015 -> "Modulex Lemon",
    30016 -> "Modulex Pink",
    30032 -> "Modulex Light Bluish Grey",
    30033 -> "Modulex Pink Red",
    30034 -> "Modulex Aqua Green",
    30035 -> "Modulex Light Yellow",
    30037 -> "Modulex Violet",
    30038 -> "Modulex Medium Blue",
    30039 -> "Modulex Light Orange",
    30040 -> "Modulex Charcoal Grey",
    30054 -> "Modulex Dark Brown",
    30080 -> "Modulex Foil White",
    30081 -> "Modulex Foil Black",
    30082 -> "Modulex Foil Dark Grey",
    30083 -> "Modulex Foil Light Grey",
    30084 -> "Modulex Foil Dark Green",
    30085 -> "Modulex Foil Light Green",
    30086 -> "Modulex Foil Dark Blue",
    30087 -> "Modulex Foil Light Blue",
    30088 -> "Modulex Foil Violet",
    30089 -> "Modulex Foil Dark Red",
    30090 -> "Modulex Foil Yellow",
    30091 -> "Modulex Foil Orange",
    31000 -> "Modulex Dark Grey",
    31001 -> "Modulex Blue",
    31002 -> "Modulex Very Light Grey"
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
      val content = Source.fromInputStream(zipFile.getInputStream(entry)).mkString
      readColoredPartsFromString(content)
    } finally {
      zipFile.close()
    }
  }

  def readColoredPartsFromString(content: String): List[ColoredPart] = {
    val (firstSubfileName, subfiles, partDescriptions) = parseAllSubfiles(content)
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

  private def parseAllSubfiles(content: String): (String, Map[String, ParsedSubfile], Map[String, String]) = {
    val subfilesMap = mutable.Map[String, ParsedSubfile]()
    val partDescriptions = mutable.Map[String, String]()
    var firstSubfileName: String = null
    
    // Split content by subfiles (0 FILE ... 0 NOFILE)
    val subfileSections = content.split("0 NOFILE")
    var hasSubfiles = false
    
    for (section <- subfileSections) {
      val trimmed = section.trim
      val cleanContent = if (trimmed.startsWith("\uFEFF")) trimmed.drop(1) else trimmed
      if (cleanContent.startsWith("0 FILE")) {
        hasSubfiles = true
        // Extract subfile name from "0 FILE filename"
        val lines = trimmed.linesIterator.toVector
        val nameLine = lines.head.trim
        val subfileName = nameLine.replaceFirst("0 FILE", "").trim
        
        if (subfileName.nonEmpty) {
          if (firstSubfileName == null) {
            firstSubfileName = subfileName
          }
          
          // Extract part description if this is a .dat subfile
          if (subfileName.endsWith(".dat") || subfileName.endsWith(".DAT")) {
            if (lines.length > 1) {
              val secondLine = lines(1).trim
              if (secondLine.startsWith("0 ") && !secondLine.startsWith("0 Name:")) {
                val description = secondLine.stripPrefix("0 ")
                val partNumber = extractPartNumber(subfileName)
                partDescriptions(partNumber) = description
              }
            }
          }
          
          val parsed = parseSubfile(subfileName, trimmed)
          subfilesMap(subfileName.toLowerCase) = parsed
        }
      }
    }
    
    // If no subfile structure found, treat entire content as main subfile
    if (!hasSubfiles && content.trim.nonEmpty) {
      val parsed = parseSubfile("main", content)
      subfilesMap("main") = parsed
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
