package com.wolfskeep

import akka.actor.typed.{ActorRef, Behavior, ActorSystem}
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.AskPattern._
import akka.util.Timeout
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

object PartsProcessor {
  sealed trait Command
  case class ProcessParts(coloredParts: List[ColoredPart], replyTo: ActorRef[Response]) extends Command

  sealed trait Response
  case class ProcessedParts(parts: List[MatchedPart]) extends Response

  def apply(
    taxonomyDataHolder: ActorRef[TaxonomyDataHolder.Command],
    downloader: ActorRef[CachedDownloader.Command],
    bricklinkActor: ActorRef[BricklinkActor.Command]
  ): Behavior[Command] = {
    Behaviors.setup { context =>
      implicit val ec: ExecutionContext = context.executionContext
      implicit val scheduler: akka.actor.typed.Scheduler = context.system.scheduler
      val logger = context.log
      
      Behaviors.receiveMessage { message =>
        message match {
          case ProcessParts(coloredParts, replyTo) =>
            logger.info(s"ProcessParts with ${coloredParts.size} input parts")
            val taxonomyTimeout: Timeout = Timeout(1.seconds)
            val taxonomyDataFuture = taxonomyDataHolder.ask(ref => TaxonomyDataHolder.GetTaxonomy(ref))(taxonomyTimeout, scheduler)

            taxonomyDataFuture.onComplete {
              case scala.util.Success(data: TaxonomyDataHolder.TaxonomyData) =>
                val parts = data.parts
                val partMap = parts.flatMap(part => (part.partNumber :: part.altNumbers.toList).map(_ -> part)).toMap

                val (matched, unmatched) = matchByPartNumber(coloredParts, partMap)

                if (unmatched.isEmpty) {
                  logger.info(s"ProcessParts replying with ${matched.size} output parts")
                  replyTo ! ProcessedParts(matched.sorted)
                } else {
                  implicit val bricksetTimeout: Timeout = Timeout(30.seconds)
                  
                  val resultFuture = for {
                    withBricksetData <- fetchBricksetDetails(unmatched, downloader)
                    bricklinkResults <- lookupBricklinkAndMatch(withBricksetData, parts, bricklinkActor)
                    (withBricklinkMatch, stillWithoutCategories) = bricklinkResults.partition(_.legoPart.exists(_.categories.nonEmpty))
                    fuzzyMatched <- inferCategoriesByName(stillWithoutCategories, matched ++ withBricklinkMatch, taxonomyDataHolder, logger)
                  } yield { matched ++ withBricklinkMatch ++ fuzzyMatched }
                  
                  resultFuture.onComplete {
                    case scala.util.Success(allMatched) =>
                      val sortedParts = allMatched.sorted
                      logger.info(s"ProcessParts replying with ${sortedParts.size} output parts")
                      replyTo ! ProcessedParts(sortedParts)
                    case scala.util.Failure(ex) =>
                      logger.error(s"Failed to process parts: ${ex.getMessage}")
                      val sortedParts = (matched ++ unmatched).sorted
                      logger.info(s"ProcessParts replying with ${sortedParts.size} output parts")
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
  )(implicit timeout: Timeout, ec: ExecutionContext, scheduler: akka.actor.typed.Scheduler): Future[List[MatchedPart]] = {
    if (unmatchedParts.isEmpty) Future.successful(Nil)
    else {
      val futures: List[Future[MatchedPart]] = unmatchedParts.map { mp =>
        BricksetPartFetcher.fetchPartDetails(
          downloader,
          mp.coloredPart.partNumber,
          mp.coloredPart.elementId
        ).map { resultOpt =>
          resultOpt.map { result =>
            val updatedColoredPart = mp.coloredPart.copy(
              elementId = result.elementId.orElse(mp.coloredPart.elementId)
            )
            MatchedPart(updatedColoredPart, Some(LegoPart(
              partNumber = "", 
              name = "", 
              categories = Nil,
              sequenceNumber = 0,
              altNumbers = Set.empty,
              imageUrl = result.imageUrl,
              imageWidth = None,
              imageHeight = None
            )), false)
          }.getOrElse(mp)
        }
      }
      Future.sequence(futures)
    }
  }

  private def lookupBricklinkAndMatch(
    partsWithoutCategories: List[MatchedPart],
    taxonomyParts: List[LegoPart],
    bricklinkActor: ActorRef[BricklinkActor.Command]
  )(implicit timeout: Timeout, ec: ExecutionContext, scheduler: akka.actor.typed.Scheduler): Future[List[MatchedPart]] = {
    val partsWithElementId = partsWithoutCategories.filter(_.coloredPart.elementId.isDefined)
    
    if (partsWithElementId.isEmpty) {
      Future.successful(partsWithoutCategories)
    } else {
      val futures: List[Future[MatchedPart]] = partsWithElementId.map { mp =>
        val elemId = mp.coloredPart.elementId.get
        bricklinkActor.ask(BricklinkActor.GetItemNumberByElementId(elemId, _)).map {
          case BricklinkActor.ItemMappingResponse(itemNo, itemType) =>
            val taxonomyMatch = BricksetPartFetcher.matchBricklinkItemToTaxonomy(itemNo, taxonomyParts)
            taxonomyMatch.map { tp =>
              MatchedPart(
                mp.coloredPart,
                mp.legoPart.map { lp => tp.copy(/* partNumber = "", */ name = tp.name + " (modified)", imageUrl = lp.imageUrl, imageWidth = None, imageHeight = None) }
                  .orElse(Some(tp)),
                categoriesGuessed = false
              )
            }.getOrElse(mp)
          case BricklinkActor.Failed(message) =>
            mp
        }
      }
      Future.sequence(futures).map { results =>
        val matchedPartNumbers = results.filter(_.legoPart.isDefined).map(_.coloredPart.partNumber).toSet
        val unmatched = partsWithoutCategories.filterNot(mp => matchedPartNumbers.contains(mp.coloredPart.partNumber))
        results ++ unmatched
      }
    }
  }

  private def inferCategoriesByName(
    partsWithoutCategories: List[MatchedPart],
    matchedParts: List[MatchedPart],
    taxonomyDataHolder: ActorRef[TaxonomyDataHolder.Command],
    logger: org.slf4j.Logger
  )(implicit timeout: Timeout, scheduler: akka.actor.typed.Scheduler, ec: ExecutionContext): Future[List[MatchedPart]] = {
    val futures: List[Future[MatchedPart]] = partsWithoutCategories.map { mp =>
      val searchTimeout: Timeout = Timeout(5.seconds)
      taxonomyDataHolder.ask(ref => TaxonomyDataHolder.SearchByName(mp.coloredPart.name, ref))(searchTimeout, scheduler).map {
        case TaxonomyDataHolder.SearchResult(searchResults) =>
          val (updatedMatchedPart, _) = inferCategories(mp, searchResults, matchedParts, logger)
          updatedMatchedPart
      }
    }
    Future.sequence(futures)
  }

  private def inferCategories(
    mp: MatchedPart,
    searchResults: List[(LegoPart, Int)],
    matchedParts: List[MatchedPart],
    logger: org.slf4j.Logger
  ): (MatchedPart, Boolean) = {
    val coloredPartName = mp.coloredPart.name
    
    logger.debug(s"Fuzzy matching: partNumber=${mp.coloredPart.partNumber}, name=$coloredPartName")
    val wordsTokenized = PartNameIndex.tokenize(coloredPartName)
    logger.debug(s"  Tokenized: ${wordsTokenized.mkString(", ")}")
    logger.debug(s"  Top 5 fuzzy matches: ${searchResults.take(5).map { case (part, count) => s"${part.name} ($count)" }.mkString(", ")}")

    if (searchResults.isEmpty) {
      return (mp, false)
    }

    val bricksetWords = wordsTokenized.toSet

    val exactMatch = searchResults.find { case (part, _) =>
      val partWords = PartNameIndex.tokenize(part.name).toSet
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
        val commonPrefix = PartNameIndex.findCommonCategoryPrefix(top5.map(_._1))
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
