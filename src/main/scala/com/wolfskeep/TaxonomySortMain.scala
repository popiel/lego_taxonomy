package com.wolfskeep

import akka.actor.typed.ActorRef
import akka.actor.typed.ActorSystem
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.AskPattern._
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import com.wolfskeep.rebrickable.{RebrickableDataActor, RebrickableFetcherActor, RebrickableSchedulerActor}
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext

object TaxonomySortMain {

  def main(args: Array[String]): Unit = {
    if (args.contains("-web")) {
      runWebMode()
    } else {
      runBatchMode(args)
    }
  }

  def runWebMode(): Unit = {
    val config = ConfigFactory.parseString("""
        akka.http.parsing.cookie-parsing-mode = raw
      """).withFallback(ConfigFactory.load())

    val system: ActorSystem[TaxonomyFetcher.Command] = ActorSystem(TaxonomyFetcher(), "taxonomy-fetcher-system", config)

    val taxonomyDataHolder = system.systemActorOf(TaxonomyDataHolder(), "taxonomy-data-holder")

    val rebrickableData = system.systemActorOf(RebrickableDataActor(), "rebrickable-data")

    val cache = system.systemActorOf(DiskCache(), "cache")
    val downloader = system.systemActorOf(CachedDownloader(cache), "downloader")

    val partsProcessor = system.systemActorOf(PartsProcessor(taxonomyDataHolder, downloader, rebrickableData), "parts-processor")

    val taxonomyScheduler = system.systemActorOf(TaxonomyScheduler(system, taxonomyDataHolder), "taxonomy-scheduler")
    taxonomyScheduler ! TaxonomyScheduler.FetchTaxonomy

    val rebrickableFetcher = system.systemActorOf(RebrickableFetcherActor(rebrickableData), "rebrickable-fetcher")
    val rebrickableScheduler = system.systemActorOf(RebrickableSchedulerActor(rebrickableFetcher, rebrickableData), "rebrickable-scheduler")
    rebrickableScheduler ! RebrickableSchedulerActor.FetchRebrickable

    import system.executionContext
    import akka.stream.Materializer
    implicit val materializer: Materializer = Materializer(system)

    val bindingFuture = HttpServer.start(partsProcessor, rebrickableData, system)

    Await.result(system.whenTerminated, Duration.Inf)
    System.exit(0)
  }

  def runBatchMode(args: Array[String]): Unit = {
    val system: ActorSystem[TaxonomyFetcher.Command] = ActorSystem(TaxonomyFetcher(), "taxonomy-fetcher-system")
    val probe = system.systemActorOf(Behaviors.receiveMessage[TaxonomyFetcher.Response] { msg =>
      msg match {
        case TaxonomyFetcher.TaxonomyFetched(taxonomyData) =>
          val catCsv = buildCategoriesCsv(taxonomyData.categories)
          val partCsv = buildPartsCsv(taxonomyData.parts)
          writeToFile("categories.csv", catCsv)
          writeToFile("parts.csv", partCsv)
          system.log.info("CSVs written to files, now processing inventories")
          processInventories(taxonomyData, args)
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

  def matchParts(coloredParts: List[ColoredPart], taxonomyData: TaxonomyData): List[MatchedPart] = {
    val matchedParts = coloredParts.map { cp =>
      val legoPart = taxonomyData.findPart(cp.partNumber)
      MatchedPart(cp, legoPart)
    }
    matchedParts.sorted
  }

  def processInventories(taxonomyData: TaxonomyData, files: Array[String]): Unit = {
    for (file <- files) {
      if (file.endsWith(".csv")) {
        val coloredParts = new CsvReader().readColoredParts(file)
        val matchedParts = matchParts(coloredParts, taxonomyData)

        val outputFile = file.replace(".csv", "-sorted.csv")
        val header = "quantity,color,partNumber_input,name_input,partNumber_taxonomy,name_taxonomy,category,category2,category3,category4\n"
        val rows = matchedParts.map { mp =>
          val name_taxonomy = mp.legoPart.map(_.name).getOrElse("")
          val partNumber_taxonomy = mp.legoPart.map(_.partNumber).getOrElse("")
          val catNames = mp.legoPart.map(_.categories.map(_.name)).getOrElse(Nil)
          s"${mp.coloredPart.quantity},${escapeCsv(mp.coloredPart.color)},${mp.coloredPart.partNumber},${escapeCsv(mp.coloredPart.name)},${partNumber_taxonomy},${escapeCsv(name_taxonomy)},${catNames.headOption.getOrElse("")},${catNames.lift(1).getOrElse("")},${catNames.lift(2).getOrElse("")},${catNames.lift(3).getOrElse("")}"
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
