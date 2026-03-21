package com.wolfskeep.rebrickable

import java.io.InputStream
import java.nio.file.{Files, Paths}
import java.util.zip.ZipInputStream

case class Color(
  id: Int,
  name: String,
  rgb: String,
  isTrans: Boolean,
  numParts: Int,
  numSets: Int,
  yearFrom: Int,
  yearTo: Int
)

case class Part(
  partNum: String,
  name: String,
  partCatId: Int,
  partMaterial: String
)

case class Element(
  elementId: Long,
  partNum: String,
  colorId: Int,
  designId: Option[Int]
)

case class RebrickableSet(
  setNum: String,
  name: String,
  year: Int,
  themeId: Int,
  numParts: Int,
  imgUrl: Option[String]
)

case class Inventory(
  id: Int,
  version: Int,
  setNum: String
)

case class InventoryPart(
  inventoryId: Int,
  partNum: String,
  colorId: Int,
  quantity: Int,
  isSpare: Boolean,
  imgUrl: Option[String]
)

case class Data(
  colors: List[Color],
  parts: List[Part],
  elements: List[Element],
  sets: List[RebrickableSet],
  inventories: List[Inventory],
  inventoryParts: List[InventoryPart]
) {
  lazy val elementIdToElement: Map[Long, Element] = elements.map(e => e.elementId -> e).toMap
  lazy val partNumColorIdToElement: Map[(String, Int), Element] = elements.map(e => (e.partNum, e.colorId) -> e).toMap
  lazy val partNumToPart: Map[String, Part] = parts.map(p => p.partNum -> p).toMap
  lazy val colorIdToColor: Map[Int, Color] = colors.map(c => c.id -> c).toMap
  lazy val setNumToSet: Map[String, RebrickableSet] = sets.map(s => s.setNum -> s).toMap
  lazy val setNumToInventory: Map[String, List[Inventory]] = inventories.groupBy(_.setNum)
  lazy val inventoryIdToParts: Map[Int, List[InventoryPart]] = inventoryParts.groupBy(_.inventoryId)

  def elementIdToDesignId(elementId: Long): Option[String] =
    elementIdToElement.get(elementId).flatMap(_.designId.map(_.toString))

  def elementByPartNumAndColorId(partNum: String, colorId: Int): Option[Element] =
    partNumColorIdToElement.get((partNum, colorId))
}

object Color {
  private val ZipFileName = "colors.csv.zip"
  private val CsvFileName = "colors.csv"
  private val ExpectedColumns = Array("id", "name", "rgb", "is_trans", "num_parts", "num_sets", "y1", "y2")

  def fromZip(): List[Color] = {
    Data.findInputStream(ZipFileName) match {
      case Some(is) =>
        try Data.readZipWithHeader(is, CsvFileName, ExpectedColumns)(parseLine)
        finally is.close()
      case None => Nil
    }
  }

  private def parseLine(fields: Array[String]): Option[Color] = {
    if (fields.length < 8) None
    else {
      for {
        id <- fields(0).toIntOption
        name = fields(1)
        rgb = fields(2)
        isTrans <- fields(3).toBooleanOption
        numParts <- fields(4).toIntOption
        numSets <- fields(5).toIntOption
        yearFrom <- fields(6).toIntOption
        yearTo <- fields(7).toIntOption
      } yield Color(id, name, rgb, isTrans, numParts, numSets, yearFrom, yearTo)
    }
  }
}

object Part {
  private val ZipFileName = "parts.csv.zip"
  private val CsvFileName = "parts.csv"
  private val ExpectedColumns = Array("part_num", "name", "part_cat_id", "part_material")

  def fromZip(): List[Part] = {
    Data.findInputStream(ZipFileName) match {
      case Some(is) =>
        try Data.readZipWithHeader(is, CsvFileName, ExpectedColumns)(parseLine)
        finally is.close()
      case None => Nil
    }
  }

  private def parseLine(fields: Array[String]): Option[Part] = {
    if (fields.length < 4) None
    else {
      val partNum = fields(0).trim
      if (partNum.isEmpty) None
      else {
        val name = fields(1)
        val partCatId = fields(2).toIntOption.getOrElse(return None)
        val partMaterial = fields(3)
        Some(Part(partNum, name, partCatId, partMaterial))
      }
    }
  }
}

object Element {
  private val ZipFileName = "elements.csv.zip"
  private val CsvFileName = "elements.csv"
  private val ExpectedColumns = Array("element_id", "part_num", "color_id", "design_id")

  def fromZip(): List[Element] = {
    Data.findInputStream(ZipFileName) match {
      case Some(is) =>
        try Data.readZipWithHeader(is, CsvFileName, ExpectedColumns)(parseLine)
        finally is.close()
      case None => Nil
    }
  }

  private def parseLine(fields: Array[String]): Option[Element] = {
    if (fields.length < 4) None
    else {
      for {
        elementId <- fields(0).toLongOption
        partNum = fields(1)
        colorId <- fields(2).toIntOption
        designId <- fields(3) match {
          case s if s.isEmpty => Some(None)
          case s => s.toIntOption.map(Some(_))
        }
      } yield Element(elementId, partNum, colorId, designId)
    }
  }
}

object RebrickableSet {
  private val ZipFileName = "sets.csv.zip"
  private val CsvFileName = "sets.csv"
  private val ExpectedColumns = Array("set_num", "name", "year", "theme_id", "num_parts", "img_url")

  def fromZip(): List[RebrickableSet] = {
    Data.findInputStream(ZipFileName) match {
      case Some(is) =>
        try Data.readZipWithHeader(is, CsvFileName, ExpectedColumns)(parseLine)
        finally is.close()
      case None => Nil
    }
  }

  private def parseLine(fields: Array[String]): Option[RebrickableSet] = {
    if (fields.length < 6) None
    else {
      val setNum = fields(0).trim
      if (setNum.isEmpty) None
      else {
        val name = fields(1)
        val year = fields(2).toIntOption.getOrElse(return None)
        val themeId = fields(3).toIntOption.getOrElse(return None)
        val numParts = fields(4).toIntOption.getOrElse(return None)
        val imgUrl = if (fields(5).nonEmpty) Some(fields(5)) else None
        Some(RebrickableSet(setNum, name, year, themeId, numParts, imgUrl))
      }
    }
  }
}

object Inventory {
  private val ZipFileName = "inventories.csv.zip"
  private val CsvFileName = "inventories.csv"
  private val ExpectedColumns = Array("id", "version", "set_num")

  def fromZip(): List[Inventory] = {
    Data.findInputStream(ZipFileName) match {
      case Some(is) =>
        try Data.readZipWithHeader(is, CsvFileName, ExpectedColumns)(parseLine)
        finally is.close()
      case None => Nil
    }
  }

  private def parseLine(fields: Array[String]): Option[Inventory] = {
    if (fields.length < 3) None
    else {
      for {
        id <- fields(0).toIntOption
        version <- fields(1).toIntOption
        setNum = fields(2)
      } yield Inventory(id, version, setNum)
    }
  }
}

object InventoryPart {
  private val ZipFileName = "inventory_parts.csv.zip"
  private val CsvFileName = "inventory_parts.csv"
  private val ExpectedColumns = Array("inventory_id", "part_num", "color_id", "quantity", "is_spare", "img_url")

  def fromZip(): List[InventoryPart] = {
    Data.findInputStream(ZipFileName) match {
      case Some(is) =>
        try Data.readZipWithHeader(is, CsvFileName, ExpectedColumns)(parseLine)
        finally is.close()
      case None => Nil
    }
  }

  private def parseLine(fields: Array[String]): Option[InventoryPart] = {
    if (fields.length < 6) None
    else {
      for {
        inventoryId <- fields(0).toIntOption
        partNum = fields(1)
        colorId <- fields(2).toIntOption
        quantity <- fields(3).toIntOption
        isSpare <- fields(4).toBooleanOption
        imgUrl = if (fields(5).nonEmpty) Some(fields(5)) else None
      } yield InventoryPart(inventoryId, partNum, colorId, quantity, isSpare, imgUrl)
    }
  }
}

object Data {
  def load(): Data = {
    import scala.concurrent.{Await, Future}
    import scala.concurrent.duration._
    import scala.concurrent.ExecutionContext.Implicits.global

    val partsFuture: Future[List[Part]] = Future(Part.fromZip())
    val colorsFuture: Future[List[Color]] = Future(Color.fromZip())
    val elementsFuture: Future[List[Element]] = Future(Element.fromZip())
    val setsFuture: Future[List[RebrickableSet]] = Future(RebrickableSet.fromZip())
    val inventoriesFuture: Future[List[Inventory]] = Future(Inventory.fromZip())
    val inventoryPartsFuture: Future[List[InventoryPart]] = Future(InventoryPart.fromZip())

    val allFutures = for {
      parts <- partsFuture
      colors <- colorsFuture
      elements <- elementsFuture
      sets <- setsFuture
      inventories <- inventoriesFuture
      inventoryParts <- inventoryPartsFuture
    } yield Data(colors, parts, elements, sets, inventories, inventoryParts)

    Await.result(allFutures, 5.minutes)
  }

  def findInputStream(name: String): Option[InputStream] = {
    val cachePath = Paths.get(".cache", "rebrickable", name)
    if (Files.exists(cachePath)) {
      return Some(Files.newInputStream(cachePath))
    }

    val resourcePath = Paths.get("src", "main", "resources", "rebrickable", name)
    if (Files.exists(resourcePath)) {
      return Some(Files.newInputStream(resourcePath))
    }

    Option(getClass.getResourceAsStream(s"/rebrickable/$name"))
  }

  def readZipWithHeader[T](is: InputStream, csvFileName: String, expectedColumns: Array[String])(
    parseLine: Array[String] => Option[T]
  ): List[T] = {
    val zis = new ZipInputStream(is)
    val buffer = scala.collection.mutable.ListBuffer[T]()
    val reader = new java.io.BufferedReader(new java.io.InputStreamReader(zis, java.nio.charset.StandardCharsets.UTF_8))
    try {
      var entry = zis.getNextEntry
      while (entry != null) {
        if (entry.getName == csvFileName) {
          val headerLine = reader.readLine()
          if (headerLine != null) {
            val headerFields = parseCsvLine(headerLine)
            val headerMap = headerFields.zipWithIndex.toMap
            val permutation = expectedColumns.map(col => headerMap.getOrElse(col, -1))

            var line = reader.readLine()
            while (line != null) {
              val fields = parseCsvLine(line)
              val reordered = permutation.map(idx => if (idx >= 0 && idx < fields.length) fields(idx) else "")
              parseLine(reordered).foreach(buffer += _)
              line = reader.readLine()
            }
          }
        }
        entry = zis.getNextEntry
      }
    } finally {
      reader.close()
    }
    buffer.toList
  }

  def parseCsvLine(line: String): List[String] = {
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
      } else {
        sb.append(c)
      }
      i += 1
    }
    fields = sb.toString :: fields
    fields.reverse
  }
}
