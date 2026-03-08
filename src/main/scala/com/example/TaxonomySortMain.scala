package com.example

//#imports
import akka.actor.typed.ActorRef
import akka.actor.typed.ActorSystem
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import scala.concurrent.Await
import scala.concurrent.duration.Duration
//#imports


//#hello-world-main
object TaxonomySortMain {

  //#hello-world-main
  def main(args: Array[String]): Unit = {
    val system: ActorSystem[TaxonomyFetcher.Command] = ActorSystem(TaxonomyFetcher(), "taxonomy-fetcher-system")
    val probe = system.systemActorOf(Behaviors.receiveMessage[TaxonomyFetcher.Response] { msg =>
      msg match {
        case TaxonomyFetcher.TaxonomyFetched(categories, parts) =>
          val catCsv = buildCategoriesCsv(categories)
          val partCsv = buildPartsCsv(parts)
          writeToFile("categories.csv", catCsv)
          writeToFile("parts.csv", partCsv)
          system.log.info("CSVs written to files, terminating system")
          system.terminate()
        case TaxonomyFetcher.Failed(reason) =>
          system.log.error(s"taxonomy fetch failed: ${reason.getMessage}", reason)
          system.terminate()
      }
      Behaviors.stopped
    }, "probe")

    system ! TaxonomyFetcher.GetTaxonomy(probe)

    Await.result(system.whenTerminated, Duration.Inf)
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
    val sortedParts = parts.filter(_.name != "").sortWith { (a, b) =>
      val aCats = a.categories.map(_.number.toInt)
      val bCats = b.categories.map(_.number.toInt)
      val minLen = math.min(aCats.length, bCats.length)
      val cmp = (0 until minLen).view.map(i => aCats(i).compare(bCats(i))).find(_ != 0).getOrElse(aCats.length.compare(bCats.length))
      cmp < 0
    }
    val rows = sortedParts.map { part =>
      val catNames = part.categories.map(_.name)
      s"${part.partNumber},${escapeCsv(part.name)},${catNames.headOption.getOrElse("")},${catNames.lift(1).getOrElse("")},${catNames.lift(2).getOrElse("")},${catNames.lift(3).getOrElse("")}"
    }.mkString("\n")
    header + rows
  }

  def escapeCsv(s: String): String = if (s.contains(",") || s.contains("\"") || s.contains("\n")) s"""\"${s.replace("\"", "\"\"")}\"""" else s

  def writeToFile(filename: String, content: String): Unit = {
    import java.nio.file.{Files, Paths}
    Files.write(Paths.get(filename), content.getBytes)
  }

  //#hello-world-main
}
//#hello-world-main
