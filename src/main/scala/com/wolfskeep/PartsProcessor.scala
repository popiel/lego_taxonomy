package com.wolfskeep

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.AskPattern._
import akka.util.Timeout
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Try
import com.wolfskeep.rebrickable.RebrickableHolder
import com.wolfskeep.rebrickable.LDrawImageFetcherTrait

object PartsProcessor {
  sealed trait Command
  case class ProcessParts(coloredParts: List[ColoredPart], replyTo: ActorRef[Response]) extends Command

  sealed trait Response
  case class ProcessedParts(parts: List[MatchedPart]) extends Response

  def apply(
    taxonomyDataHolder: ActorRef[TaxonomyHolder.Command],
    downloader: ActorRef[CachedDownloader.Command],
    rebrickableDataActor: ActorRef[RebrickableHolder.Command],
    ldrawImageFetcher: LDrawImageFetcherTrait
  ): Behavior[Command] = {
    Behaviors.setup { context =>
      implicit val ec: ExecutionContext = context.executionContext
      implicit val scheduler: akka.actor.typed.Scheduler = context.system.scheduler
      val logger = context.log

      Behaviors.receiveMessage {
        case ProcessParts(coloredParts, replyTo) =>
          logger.info(s"ProcessParts with ${coloredParts.size} input parts")
          implicit val taxonomyTimeout: Timeout = Timeout(1.seconds)

          val taxonomyDataFuture = taxonomyDataHolder.ask(ref => TaxonomyHolder.GetTaxonomy(ref))(taxonomyTimeout, scheduler)

          taxonomyDataFuture.onComplete {
            case scala.util.Success(data: TaxonomyHolder.TaxonomyDataResponse) =>
              val taxonomyData = data.taxonomyData

              val (matched, unmatched) = coloredParts.map(part => MatchedPart(part, taxonomyData.findPart(part.partNumber), false))
                .partition(_.legoPart.nonEmpty)

              if (unmatched.isEmpty) {
                logger.info(s"ProcessParts replying with ${matched.size} output parts (Brickset: 0, BrickLink: 0, Rebrickable: 0)")
                replyTo ! ProcessedParts(matched.sorted)
              } else {
                processUnmatchedParts(matched, unmatched, taxonomyData, rebrickableDataActor, downloader, ldrawImageFetcher, replyTo)(ec, scheduler, logger)
              }

            case scala.util.Failure(ex) =>
              logger.error(s"Failed to get taxonomy data: ${ex.getMessage}")
              replyTo ! ProcessedParts(Nil)
          }

          Behaviors.same
      }
    }
  }

  private def processUnmatchedParts(
    matched: List[MatchedPart],
    unmatched: List[MatchedPart],
    taxonomyData: TaxonomyData,
    rebrickableDataActor: ActorRef[RebrickableHolder.Command],
    downloader: ActorRef[CachedDownloader.Command],
    ldrawImageFetcher: LDrawImageFetcherTrait,
    replyTo: ActorRef[ProcessedParts]
  )(implicit ec: ExecutionContext, scheduler: akka.actor.typed.Scheduler, logger: org.slf4j.Logger): Unit = {
    implicit val rebrickableTimeout: Timeout = Timeout(30.seconds)

    val rebrickableDataFuture = rebrickableDataActor.ask(RebrickableHolder.GetData(_))(rebrickableTimeout, scheduler)

    rebrickableDataFuture.onComplete {
      case scala.util.Success(rebrickableData) =>
        val futures: List[Future[MatchedPart]] = unmatched.map { mp =>
          processSinglePart(mp, taxonomyData, rebrickableData, downloader, ldrawImageFetcher)
        }
        val sequenceFuture: Future[List[MatchedPart]] = Future.sequence(futures)
        sequenceFuture.onComplete {
          case scala.util.Success(processedResults: List[MatchedPart]) =>
            val allMatchedSoFar: List[MatchedPart] = matched ++ processedResults
            val (withCategories, withoutCategories) = allMatchedSoFar.partition(_.legoPart.exists(_.categories.nonEmpty))
            val fuzzyMatched = inferCategoriesByName(withoutCategories, withCategories, taxonomyData, logger)
            val sortedParts = (withCategories ++ fuzzyMatched).sorted
            logger.info(s"ProcessParts replying with ${sortedParts.size} output parts")
            replyTo ! ProcessedParts(sortedParts)

          case scala.util.Failure(ex) =>
            logger.error(s"Failed to process parts: ${ex.getMessage}")
            val sortedParts = (matched ++ unmatched).sorted
            logger.info(s"ProcessParts replying with ${sortedParts.size} output parts")
            replyTo ! ProcessedParts(sortedParts)
        }

      case scala.util.Failure(ex) =>
        logger.error(s"Failed to get rebrickable data: ${ex.getMessage}")
        val sortedParts = (matched ++ unmatched).sorted
        logger.info(s"ProcessParts replying with ${sortedParts.size} output parts")
        replyTo ! ProcessedParts(sortedParts)
    }
  }

  private def createGuessedLegoPart(name: String, categories: List[Category]): LegoPart =
    LegoPart(
      partNumber = "",
      name = name,
      categories = categories,
      sequenceNumber = 0,
      altNumbers = Set.empty,
      imageWidth = None,
      imageHeight = None,
      imageUrl = None
    )

  private def processSinglePart(
    matchedPart: MatchedPart,
    taxonomyData: TaxonomyData,
    rebrickableData: com.wolfskeep.rebrickable.Data,
    downloader: ActorRef[CachedDownloader.Command],
    ldrawImageFetcher: LDrawImageFetcherTrait
  )(implicit ec: ExecutionContext, scheduler: akka.actor.typed.Scheduler, logger: org.slf4j.Logger): Future[MatchedPart] = {
    val coloredPart = matchedPart.coloredPart
    val originalElementId = coloredPart.elementId

    val colorNameToId = rebrickableData.colors.map(c => c.name -> c.id).toMap

    val currentElementId = originalElementId
    val elemIdStr = currentElementId.getOrElse("")
    val designIdOpt: Option[String] = if (elemIdStr.nonEmpty) {
      Try(elemIdStr.toLong).toOption.flatMap(rebrickableData.elementIdToDesignId)
    } else None

    val taxonomyMatch: Option[LegoPart] = taxonomyData.findBasePart(coloredPart.partNumber)
      .orElse(designIdOpt.flatMap(taxonomyData.findBasePart))

    val ldImageUrl = Try(findPartImageUrl(coloredPart, colorNameToId, ldrawImageFetcher)).getOrElse(None)

    val bricksetTimeout: Timeout = Timeout(5.seconds)

    val imageUrlFuture = ldImageUrl match {
      case Some(url) => Future.successful(Some(url))
      case None =>
        Try(
          BricksetPartFetcher.fetchPartDetails(
            downloader,
            coloredPart.partNumber,
            originalElementId
          )(bricksetTimeout, scheduler, ec).map { bricksetResult =>
            bricksetResult.flatMap(_.imageUrl)
          }.recover { case _ => None }
        ).getOrElse(Future.successful(None))
    }

    imageUrlFuture.map { imageUrl =>
      taxonomyMatch match {
        case Some(tp) =>
          val newLegoPart = tp.copy(
            partNumber = coloredPart.partNumber,
            imageUrl = imageUrl,
            imageWidth = None,
            imageHeight = None
          )
          matchedPart.copy(legoPart = Some(newLegoPart))
        case None =>
          logger.warn(s"Failed to match element $elemIdStr, part number ${coloredPart.partNumber}, design ${designIdOpt}: ${coloredPart.name}")
          matchedPart.copy(coloredPart = coloredPart.copy(elementId = currentElementId))
      }
    }
  }

  private def findPartImageUrl(
    coloredPart: ColoredPart,
    colorNameToId: Map[String, Int],
    ldrawImageFetcher: LDrawImageFetcherTrait
  )(implicit ec: ExecutionContext): Option[String] = {
    val colorIdOpt = colorNameToId.get(coloredPart.color)

    val rebrickableColorName = if (colorIdOpt.isEmpty) {
      StudioIoReader.colorMap.values.find { name =>
        name.equalsIgnoreCase(coloredPart.color)
      }
    } else {
      None
    }

    val finalColorId = colorIdOpt.orElse {
      rebrickableColorName.flatMap(name => colorNameToId.get(name))
    }

    finalColorId.flatMap { colorId =>
      val downloaded = ldrawImageFetcher.ensureDownloaded(colorId)
      if (!downloaded) {
        None
      } else {
        val hasImage = ldrawImageFetcher.hasImageInZip(colorId, coloredPart.partNumber)
        if (hasImage) {
          Some(s"part_images/$colorId/${coloredPart.partNumber}.png")
        } else {
          None
        }
      }
    }
  }

  private def inferCategoriesByName(
    partsWithoutCategories: List[MatchedPart],
    matchedParts: List[MatchedPart],
    taxonomyData: TaxonomyData,
    logger: org.slf4j.Logger
  ): List[MatchedPart] = {
    partsWithoutCategories.map { mp =>
      val searchResults = taxonomyData.searchByName(mp.coloredPart.name)
      val (updatedMatchedPart, _) = inferCategories(mp, searchResults, matchedParts, taxonomyData, logger)
      updatedMatchedPart
    }
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
        val updatedLegoPart = mp.legoPart.getOrElse(createGuessedLegoPart(newName, matchedPart.categories))
          .copy(name = newName, categories = matchedPart.categories)
        (MatchedPart(mp.coloredPart, Some(updatedLegoPart), categoriesGuessed = true), true)

      case None =>
        val top5 = searchResults.take(5)
        val commonPrefix = taxonomyData.findCommonCategoryPrefix(top5.map(_._1))
        if (commonPrefix.nonEmpty) {
          val bestMatch = top5.find { case (part, _) =>
            part.categories.zip(commonPrefix).forall { case (cat, prefixCat) => cat == prefixCat }
          }
          val newName = bestMatch.map(p => s"${p._1.name} (guessed)").getOrElse(mp.coloredPart.name)
          val updatedLegoPart = mp.legoPart.getOrElse(createGuessedLegoPart(newName, commonPrefix))
            .copy(name = newName, categories = commonPrefix)
          (MatchedPart(mp.coloredPart, Some(updatedLegoPart), categoriesGuessed = true), true)
        } else {
          val bricksetNameLower = mp.coloredPart.name.toLowerCase

          val prefixMatch = matchedParts
            .flatMap(_.legoPart)
            .flatMap { legoPart =>
              matchedParts.find(mp => mp.legoPart.contains(legoPart)).map { matched =>
                (legoPart, matched.coloredPart.name)
              }
            }
            .filter { case (_, coloredPartName) =>
              bricksetNameLower.startsWith(coloredPartName.toLowerCase)
            }
            .sortBy { case (_, coloredPartName) => -coloredPartName.length }
            .headOption

          prefixMatch match {
            case Some((matchedLegoPart, _)) =>
              val newName = s"${matchedLegoPart.name} (guessed)"
              val updatedLegoPart = mp.legoPart.getOrElse(createGuessedLegoPart(newName, matchedLegoPart.categories))
                .copy(name = newName, categories = matchedLegoPart.categories)
              (MatchedPart(mp.coloredPart, Some(updatedLegoPart), categoriesGuessed = true), true)
            case None =>
              (mp, false)
          }
        }
    }
  }
}