package com.wolfskeep

import akka.actor.typed.ActorRef
import akka.actor.typed.ActorSystem
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.AskPattern._
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import com.wolfskeep.rebrickable.{RebrickableHolder, RebrickableFetcherActor, RebrickableSchedulerActor, LDrawImageFetcher}
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

    val rebrickableData = system.systemActorOf(RebrickableHolder(), "rebrickable-data")

    val taxonomyDataHolder = system.systemActorOf(TaxonomyHolder(rebrickableData), "taxonomy-data-holder")
    system ! TaxonomyFetcher.RegisterHolder(taxonomyDataHolder)

    val cache = system.systemActorOf(DiskCache(), "cache")
    val downloader = system.systemActorOf(CachedDownloader(cache), "downloader")

    val ldrawImageFetcher = new LDrawImageFetcher()(system)
    val partsProcessor = system.systemActorOf(PartsProcessor(taxonomyDataHolder, downloader, rebrickableData, ldrawImageFetcher), "parts-processor")

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
    val rebrickableData = system.systemActorOf(RebrickableHolder(), "rebrickable-data")
    val taxonomyDataHolder = system.systemActorOf(TaxonomyHolder(rebrickableData), "taxonomy-data-holder")
    system ! TaxonomyFetcher.RegisterHolder(taxonomyDataHolder)

    import akka.actor.typed.scaladsl.AskPattern._
    implicit val timeout: Timeout = Timeout(2.minutes)
    implicit val scheduler: akka.actor.typed.Scheduler = system.scheduler

    Await.result(system.ask[TaxonomyFetcher.Response](replyTo => TaxonomyFetcher.GetTaxonomy(replyTo)), Duration("2 minutes")) match {
      case TaxonomyFetcher.AugmentationComplete =>
        system.log.info("Taxonomy cycle complete, now processing inventories")
        processInventories(taxonomyDataHolder, args)
        system.log.info("Inventories processed, terminating system")
      case TaxonomyFetcher.Failed(reason) =>
        system.log.error(s"taxonomy fetch failed: ${reason.getMessage}", reason)
    }

    system.terminate()
    Await.result(system.whenTerminated, Duration("30 seconds"))
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

  def processInventories(taxonomyDataHolder: ActorRef[TaxonomyHolder.Command], files: Array[String])(implicit timeout: Timeout, scheduler: akka.actor.typed.Scheduler): Unit = {
    for (file <- files) {
      if (file.endsWith(".csv")) {
        val coloredParts = new CsvReader().readColoredParts(file)
        val requests = coloredParts.map(cp => TaxonomyHolder.LookupPartRequest(cp.partNumber, cp.elementId, cp.name))
        val results = Await.result(
          taxonomyDataHolder.ask(ref => TaxonomyHolder.LookupParts(requests, ref)),
          Duration("30 seconds")
        )
        val matchedParts = coloredParts.zip(results)
          .map { case (cp, result) => MatchedPart(cp, result.legoPart, result.categoriesGuessed) }
          .sorted

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
