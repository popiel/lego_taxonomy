package com.example

//#imports
import akka.actor.typed.ActorRef
import akka.actor.typed.ActorSystem
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import scala.concurrent.Await
import scala.concurrent.duration.Duration
//#imports


object TaxonomySortMain {

  def main(args: Array[String]): Unit = {
    val system: ActorSystem[TaxonomyFetcher.Command] = ActorSystem(TaxonomyFetcher(), "taxonomy-fetcher-system")
    val probe = system.systemActorOf(Behaviors.receiveMessage[TaxonomyFetcher.Response] { msg =>
      msg match {
        case TaxonomyFetcher.TaxonomyFetched(categories, parts) =>
          val catCsv = buildCategoriesCsv(categories)
          val partCsv = buildPartsCsv(parts)
          writeToFile("categories.csv", catCsv)
          writeToFile("parts.csv", partCsv)
          system.log.info("CSVs written to files, now processing inventories")
          processInventories(parts, args)
          system.log.info("Inventories processed, terminating system")
          system.terminate()
        case TaxonomyFetcher.Failed(reason) =>
          system.log.error(s"taxonomy fetch failed: ${reason.getMessage}", reason)
          system.terminate()
      }
      Behaviors.stopped
    }, "probe")

    system ! TaxonomyFetcher.GetTaxonomy(probe)

    Await.result(system.whenTerminated, Duration("2 minutes"))
    System.exit(0)
  }

  def getParentChain(cat: Category): List[String] = cat.number :: cat.parent.map(getParentChain).getOrElse(Nil)

  def buildCategoriesCsv(categories: Set[Category]): String = {
    val header = "number,name,parent,parent2,parent3\n"
    val rows = categories.toList.sortBy(_.number).map { cat =>
      val chain = getParentChain(cat)
      s"${cat.number},${escapeCsv(cat.name)},${chain.lift(1).getOrElse("")},${chain.lift(2).getOrElse("")},${chain.lift(3).getOrElse("")}"
    }.mkString("\n")
    header + rows
  }

  def buildPartsCsv(parts: List[LegoPart]): String = {
    val header = "partNumber,name,category,category2,category3,category4\n"
    val sortedParts = parts.filter(_.name != "").sorted
    val rows = sortedParts.map { part =>
      val catNames = part.categories.map(_.name)
      s"${part.partNumber},${escapeCsv(part.name)},${catNames.headOption.getOrElse("")},${catNames.lift(1).getOrElse("")},${catNames.lift(2).getOrElse("")},${catNames.lift(3).getOrElse("")}"
    }.mkString("\n")
    header + rows
  }

  def escapeCsv(s: String): String = if (s.contains(",") || s.contains("\"") || s.contains("\n")) s"""\"${s.replace("\"", "\"\"")}\"""" else s

  def processInventories(taxonomyParts: List[LegoPart], files: Array[String]): Unit = {
    val partMap = taxonomyParts.groupBy(_.partNumber).mapValues(_.head)
    for (file <- files) {
      if (file.endsWith(".csv")) {
        val coloredParts = new CsvReader().readColoredParts(file)
        val sortedParts = coloredParts.sortWith { (a, b) =>
          val aPart = partMap.get(a.partNumber)
          val bPart = partMap.get(b.partNumber)
          val partCmp = (aPart, bPart) match {
            case (Some(ap), Some(bp)) => ap.compare(bp)
            case (Some(_), None) => -1
            case (None, Some(_)) => 1
            case (None, None) => a.partNumber.compare(b.partNumber)
          }
          if (partCmp != 0) partCmp < 0
          else {
            val colorCmp = a.color.compare(b.color)
            if (colorCmp != 0) colorCmp < 0
            else a.quantity.compare(b.quantity) < 0
          }
        }
        val outputFile = file.replace(".csv", "-sorted.csv")
        val header = "quantity,color,partNumber,name,category,category2,category3,category4\n"
        val rows = sortedParts.map { cp =>
          val legoPart = partMap.get(cp.partNumber)
          val name = legoPart.map(_.name).getOrElse("")
          val catNames = legoPart.map(_.categories.map(_.name)).getOrElse(Nil)
          s"${cp.quantity},${escapeCsv(cp.color)},${cp.partNumber},${escapeCsv(name)},${catNames.headOption.getOrElse("")},${catNames.lift(1).getOrElse("")},${catNames.lift(2).getOrElse("")},${catNames.lift(3).getOrElse("")}"
        }.mkString("\n")
        writeToFile(outputFile, header + rows)
      }
    }
  }

  def writeToFile(filename: String, content: String): Unit = {
    import java.nio.file.{Files, Paths}
    Files.write(Paths.get(filename), content.getBytes)
  }

}
