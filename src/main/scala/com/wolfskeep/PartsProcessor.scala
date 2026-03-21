package com.wolfskeep

import akka.actor.typed.{ActorRef, Behavior, ActorSystem}
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.AskPattern._
import akka.util.Timeout
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import com.wolfskeep.rebrickable.RebrickableDataActor

object PartsProcessor {
  sealed trait Command
  case class ProcessParts(coloredParts: List[ColoredPart], replyTo: ActorRef[Response]) extends Command

  sealed trait Response
  case class ProcessedParts(parts: List[MatchedPart]) extends Response

  case class ProcessingTelemetry(
    bricksetRequests: Int = 0,
    bricklinkRequests: Int = 0,
    rebrickableLookups: Int = 0
  )

  def apply(
    taxonomyDataHolder: ActorRef[TaxonomyDataHolder.Command],
    downloader: ActorRef[CachedDownloader.Command],
    bricklinkActor: ActorRef[BricklinkActor.Command],
    rebrickableDataActor: ActorRef[RebrickableDataActor.Command]
  ): Behavior[Command] = {
    Behaviors.setup { context =>
      implicit val ec: ExecutionContext = context.executionContext
      implicit val scheduler: akka.actor.typed.Scheduler = context.system.scheduler
      val logger = context.log
      val rebrickableActorRef = rebrickableDataActor
      
      Behaviors.receiveMessage { message =>
        message match {
          case ProcessParts(coloredParts, replyTo) =>
            logger.info(s"ProcessParts with ${coloredParts.size} input parts")
            val taxonomyTimeout: Timeout = Timeout(1.seconds)
            val taxonomyDataFuture = taxonomyDataHolder.ask(ref => TaxonomyDataHolder.GetTaxonomy(ref))(taxonomyTimeout, scheduler)

            taxonomyDataFuture.onComplete {
              case scala.util.Success(data: TaxonomyDataHolder.TaxonomyDataResponse) =>
                val taxonomyData = data.taxonomyData

                val (matched, unmatched) = matchByPartNumber(coloredParts, taxonomyData.partNumberOrAltToPart)

                if (unmatched.isEmpty) {
                  logger.info(s"ProcessParts replying with ${matched.size} output parts (Brickset: 0, BrickLink: 0, Rebrickable: 0)")
                  replyTo ! ProcessedParts(matched.sorted)
                } else {
                  implicit val bricksetTimeout: Timeout = Timeout(30.seconds)
                  
                  val resultFuture = for {
                    (withBricksetData, telemetry) <- fetchBricksetDetails(unmatched, downloader)
                    (rebrickableResults, t2) <- lookupRebrickableAndMatch(withBricksetData, taxonomyData, rebrickableActorRef, telemetry)
                    (withRebrickableMatch, withoutMatch) = rebrickableResults.partition(_.legoPart.exists(_.categories.nonEmpty))
                    (bricklinkResults, updatedTelemetry) <- lookupBricklinkAndMatch(withoutMatch, taxonomyData, bricklinkActor, t2)
                    (withBricklinkMatch, stillWithoutCategories) = bricklinkResults.partition(_.legoPart.exists(_.categories.nonEmpty))
                    fuzzyMatched <- inferCategoriesByName(stillWithoutCategories, matched ++ withBricklinkMatch, taxonomyData, logger)
                  } yield { (matched ++ withRebrickableMatch ++ withBricklinkMatch ++ fuzzyMatched, updatedTelemetry) }
                  
                  resultFuture.onComplete {
                    case scala.util.Success((allMatched, telemetry)) =>
                      val sortedParts = allMatched.sorted
                      logger.info(s"ProcessParts replying with ${sortedParts.size} output parts (Brickset: ${telemetry.bricksetRequests}, BrickLink: ${telemetry.bricklinkRequests}, Rebrickable: ${telemetry.rebrickableLookups})")
                      replyTo ! ProcessedParts(sortedParts)
                    case scala.util.Failure(ex) =>
                      logger.error(s"Failed to process parts: ${ex.getMessage}")
                      val sortedParts = (matched ++ unmatched).sorted
                      logger.info(s"ProcessParts replying with ${sortedParts.size} output parts (Brickset: 0, BrickLink: 0, Rebrickable: 0)")
                      replyTo ! ProcessedParts(sortedParts)
                  }
                }

              case scala.util.Failure(ex) =>
                logger.error(s"Failed to get taxonomy data: ${ex.getMessage}")
                replyTo ! ProcessedParts(Nil)
            }

            Behaviors.same
        }
      }
    }
  }

  private def matchByPartNumber(
    coloredParts: List[ColoredPart],
    partMap: Map[String, LegoPart]
  ): (List[MatchedPart], List[MatchedPart]) = {
    val matchedParts = coloredParts.flatMap { cp =>
      partMap.get(cp.partNumber).map { legoPart =>
        MatchedPart(cp, Some(legoPart), categoriesGuessed = false)
      }
    }
    val unmatchedParts = coloredParts.filter(cp => !partMap.contains(cp.partNumber))
    (matchedParts, unmatchedParts.map(MatchedPart(_, None, categoriesGuessed = false)))
  }

  private def fetchBricksetDetails(
    unmatchedParts: List[MatchedPart],
    downloader: ActorRef[CachedDownloader.Command]
  )(implicit timeout: Timeout, ec: ExecutionContext, scheduler: akka.actor.typed.Scheduler): Future[(List[MatchedPart], ProcessingTelemetry)] = {
    if (unmatchedParts.isEmpty) Future.successful((Nil, ProcessingTelemetry()))
    else {
      val futures: List[Future[(MatchedPart, ProcessingTelemetry)]] = unmatchedParts.map { mp =>
        BricksetPartFetcher.fetchPartDetails(
          downloader,
          mp.coloredPart.partNumber,
          mp.coloredPart.elementId
        ).map { resultOpt =>
          resultOpt match {
            case Some(result) =>
              val updatedColoredPart = mp.coloredPart.copy(
                elementId = result.elementId.orElse(mp.coloredPart.elementId)
              )
              (MatchedPart(updatedColoredPart, Some(LegoPart(
                partNumber = "", 
                name = "", 
                categories = Nil,
                sequenceNumber = 0,
                altNumbers = Set.empty,
                imageUrl = result.imageUrl,
                imageWidth = None,
                imageHeight = None
              )), false), ProcessingTelemetry(bricksetRequests = 1))
            case None =>
              (mp, ProcessingTelemetry(bricksetRequests = 1))
          }
        }
      }
      Future.sequence(futures).map { results =>
        val matchedParts = results.map(_._1)
        val totalTelemetry = results.foldLeft(ProcessingTelemetry()) { (acc, pair) =>
          acc.copy(bricksetRequests = acc.bricksetRequests + pair._2.bricksetRequests)
        }
        (matchedParts, totalTelemetry)
      }
    }
  }

  private def lookupRebrickableAndMatch(
    partsWithoutCategories: List[MatchedPart],
    taxonomyData: TaxonomyData,
    rebrickableActor: ActorRef[RebrickableDataActor.Command],
    telemetry: ProcessingTelemetry
  )(implicit timeout: Timeout, ec: ExecutionContext, scheduler: akka.actor.typed.Scheduler): Future[(List[MatchedPart], ProcessingTelemetry)] = {
    val partsWithElementId = partsWithoutCategories.filter(_.coloredPart.elementId.isDefined)
    
    if (partsWithElementId.isEmpty) {
      Future.successful((partsWithoutCategories, telemetry))
    } else {
      val rebrickableTimeout: Timeout = Timeout(30.seconds)
      rebrickableActor.ask(RebrickableDataActor.GetData(_))(rebrickableTimeout, scheduler).map { data =>
        val results = partsWithElementId.map { mp =>
          data.elementIdToDesignId(mp.coloredPart.elementId.get.toLong).flatMap { designIdStr =>
            taxonomyData.findPart(designIdStr)
          } match {
            case Some(tp) =>
              // val existingLegoPart = mp.legoPart.getOrElse(tp)
              MatchedPart(
                mp.coloredPart,
                mp.legoPart.map { lp => tp.copy(imageUrl = lp.imageUrl, imageWidth = None, imageHeight = None) }
                  .orElse(Some(tp)),
                categoriesGuessed = false
              )
            case None => mp
          }
        }
        
        val matchedPartNumbers = results.filter(_.legoPart.isDefined)
          .map(_.coloredPart.partNumber).toSet
        val unmatched = partsWithoutCategories.filterNot(
          mp => matchedPartNumbers.contains(mp.coloredPart.partNumber)
        )
        
        (results ++ unmatched, telemetry.copy(
          rebrickableLookups = telemetry.rebrickableLookups + partsWithElementId.size
        ))
      }
    }
  }

  private def lookupBricklinkAndMatch(
    partsWithoutCategories: List[MatchedPart],
    taxonomyData: TaxonomyData,
    bricklinkActor: ActorRef[BricklinkActor.Command],
    telemetry: ProcessingTelemetry
  )(implicit timeout: Timeout, ec: ExecutionContext, scheduler: akka.actor.typed.Scheduler): Future[(List[MatchedPart], ProcessingTelemetry)] = {
    val partsWithElementId = partsWithoutCategories.filter(_.coloredPart.elementId.isDefined)
    
    if (partsWithElementId.isEmpty) {
      Future.successful((partsWithoutCategories, telemetry))
    } else {
      val futures: List[Future[(MatchedPart, ProcessingTelemetry)]] = partsWithElementId.map { mp =>
        val elemId = mp.coloredPart.elementId.get
        bricklinkActor.ask(BricklinkActor.GetItemNumberByElementId(elemId, _)).map {
          case BricklinkActor.ItemMappingResponse(itemNo, itemType) =>
            val taxonomyMatch = BricksetPartFetcher.matchBricklinkItemToTaxonomy(itemNo, taxonomyData)
            val result = taxonomyMatch match {
              case Some(tp) =>
                MatchedPart(
                  mp.coloredPart,
                  mp.legoPart.map { lp => tp.copy(imageUrl = lp.imageUrl, imageWidth = None, imageHeight = None) }
                    .orElse(Some(tp)),
                  categoriesGuessed = false
                )
              case None =>
                mp
            }
            (result, ProcessingTelemetry(bricklinkRequests = 1))
          case BricklinkActor.Failed(message) =>
            (mp, ProcessingTelemetry(bricklinkRequests = 1))
        }
      }
      Future.sequence(futures).map { results =>
        val matchedPartNumbers = results.filter(_._1.legoPart.isDefined).map(_._1.coloredPart.partNumber).toSet
        val matched = results.map(_._1)
        val unmatched = partsWithoutCategories.filterNot(mp => matchedPartNumbers.contains(mp.coloredPart.partNumber))
        val totalTelemetry = results.foldLeft(telemetry) { (acc, pair) =>
          acc.copy(bricklinkRequests = acc.bricklinkRequests + pair._2.bricklinkRequests)
        }
        (matched ++ unmatched, totalTelemetry)
      }
    }
  }

  private def inferCategoriesByName(
    partsWithoutCategories: List[MatchedPart],
    matchedParts: List[MatchedPart],
    taxonomyData: TaxonomyData,
    logger: org.slf4j.Logger
  )(implicit timeout: Timeout, scheduler: akka.actor.typed.Scheduler, ec: ExecutionContext): Future[List[MatchedPart]] = {
    val futures: List[Future[MatchedPart]] = partsWithoutCategories.map { mp =>
      Future {
        val searchResults = taxonomyData.searchByName(mp.coloredPart.name)
        val (updatedMatchedPart, _) = inferCategories(mp, searchResults, matchedParts, taxonomyData, logger)
        updatedMatchedPart
      }
    }
    Future.sequence(futures)
  }

  private def inferCategories(
    mp: MatchedPart,
    searchResults: List[(LegoPart, Int)],
    matchedParts: List[MatchedPart],
    taxonomyData: TaxonomyData,
    logger: org.slf4j.Logger
  ): (MatchedPart, Boolean) = {
    val coloredPartName = mp.coloredPart.name
    
    logger.debug(s"Fuzzy matching: partNumber=${mp.coloredPart.partNumber}, name=$coloredPartName")
    val wordsTokenized = TaxonomyData.tokenize(coloredPartName)
    logger.debug(s"  Tokenized: ${wordsTokenized.mkString(", ")}")
    logger.debug(s"  Top 5 fuzzy matches: ${searchResults.take(5).map { case (part, count) => s"${part.name} ($count)" }.mkString(", ")}")

    if (searchResults.isEmpty) {
      return (mp, false)
    }

    val bricksetWords = wordsTokenized.toSet

    val exactMatch = searchResults.find { case (part, _) =>
      val partWords = TaxonomyData.tokenize(part.name).toSet
      partWords.subsetOf(bricksetWords)
    }

    exactMatch match {
      case Some((matchedPart, _)) =>
        val newName = s"${matchedPart.name} (guessed)"
        val updatedLegoPart = mp.legoPart.getOrElse(LegoPart(
          partNumber = "",
          name = newName,
          categories = matchedPart.categories,
          sequenceNumber = 0,
          altNumbers = Set.empty,
          imageWidth = None,
          imageHeight = None,
          imageUrl = None
        )).copy(name = newName, categories = matchedPart.categories)
        (MatchedPart(mp.coloredPart, Some(updatedLegoPart), categoriesGuessed = true), true)
      case None =>
        val top5 = searchResults.take(5)
        val commonPrefix = taxonomyData.findCommonCategoryPrefix(top5.map(_._1))
        if (commonPrefix.nonEmpty) {
          val bestMatch = top5.find { case (part, _) =>
            part.categories.zip(commonPrefix).forall { case (cat, prefixCat) => cat == prefixCat }
          }
          val newName = bestMatch.map(p => s"${p._1.name} (guessed)").getOrElse(mp.coloredPart.name)
          val updatedLegoPart = mp.legoPart.getOrElse(LegoPart(
            partNumber = "",
            name = newName,
            categories = commonPrefix,
            sequenceNumber = 0,
            altNumbers = Set.empty,
            imageWidth = None,
            imageHeight = None,
            imageUrl = None
          )).copy(name = newName, categories = commonPrefix)
          (MatchedPart(mp.coloredPart, Some(updatedLegoPart), categoriesGuessed = true), true)
        } else {
          val bricksetNameLower = mp.coloredPart.name.toLowerCase

          val prefixMatch = matchedParts
            .flatMap(_.legoPart)
            .map { legoPart =>
              val matched = matchedParts.find(mp => mp.legoPart.contains(legoPart)).get
              (legoPart, matched.coloredPart.name)
            }
            .filter { case (_, coloredPartName) =>
              bricksetNameLower.startsWith(coloredPartName.toLowerCase)
            }
            .sortBy { case (_, coloredPartName) => -coloredPartName.length }
            .headOption

          prefixMatch match {
            case Some((matchedLegoPart, _)) =>
              val newName = s"${matchedLegoPart.name} (guessed)"
              val updatedLegoPart = mp.legoPart.getOrElse(LegoPart(
                partNumber = "",
                name = newName,
                categories = matchedLegoPart.categories,
                sequenceNumber = 0,
                altNumbers = Set.empty,
                imageWidth = None,
                imageHeight = None,
                imageUrl = None
              )).copy(name = newName, categories = matchedLegoPart.categories)
              (MatchedPart(mp.coloredPart, Some(updatedLegoPart), categoriesGuessed = true), true)
            case None =>
              (mp, false)
          }
        }
    }
  }
}
