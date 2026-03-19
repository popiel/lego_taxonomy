package com.wolfskeep

import akka.actor.typed.{ActorRef, Behavior, ActorSystem}
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.AskPattern._
import akka.util.Timeout
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext

object PartsProcessor {
  sealed trait Command
  case class ProcessParts(coloredParts: List[ColoredPart], replyTo: ActorRef[Response]) extends Command

  sealed trait Response
  case class ProcessedParts(parts: List[MatchedPart]) extends Response

  def apply(
    taxonomyDataHolder: ActorRef[TaxonomyDataHolder.Command],
    bricksetPartCache: ActorRef[BricksetPartCache.Command]
  ): Behavior[Command] = {
    Behaviors.setup { context =>
      implicit val ec: ExecutionContext = context.executionContext
      implicit val scheduler: akka.actor.typed.Scheduler = context.system.scheduler
      val logger = context.log
      
      Behaviors.receiveMessage { message =>
        message match {
          case ProcessParts(coloredParts, replyTo) =>
            val taxonomyTimeout: Timeout = Timeout(1.seconds)
            val taxonomyDataFuture = taxonomyDataHolder.ask(ref => TaxonomyDataHolder.GetTaxonomy(ref))(taxonomyTimeout, scheduler)

            taxonomyDataFuture.onComplete {
              case scala.util.Success(data: TaxonomyDataHolder.TaxonomyData) =>
                val parts = data.parts
                val partMap = parts.flatMap(part => (part.partNumber :: part.altNumbers.toList).map(_ -> part)).toMap
                val matchedPartsWithoutBrickset = coloredParts.map { cp =>
                  val legoPart = partMap.get(cp.partNumber)
                  MatchedPart(cp, legoPart)
                }

                val unmatchedParts = matchedPartsWithoutBrickset.filter(_.legoPart.isEmpty)
                val matchedPartsWithBrickset = matchedPartsWithoutBrickset.filter(_.legoPart.isDefined)

                if (unmatchedParts.isEmpty) {
                  val sortedParts = matchedPartsWithoutBrickset.sorted
                  replyTo ! ProcessedParts(sortedParts)
                } else {
                  val bricksetTimeout: Timeout = Timeout(30.seconds)
                  val bricksetFutures = unmatchedParts.map { mp =>
                    bricksetPartCache.ask(BricksetPartCache.GetPart(mp.coloredPart.partNumber, parts, _))(bricksetTimeout, scheduler).map { bricksetPart =>
                      (mp.coloredPart.partNumber, bricksetPart)
                    }
                  }

                  import scala.concurrent.Future
                  val allFutures: Future[Seq[(String, Option[LegoPart])]] = Future.sequence(bricksetFutures)

                  allFutures.onComplete {
                    case scala.util.Success(bricksetResults) =>
                      val bricksetMap = bricksetResults.toMap
                      val searchTimeout: Timeout = Timeout(5.seconds)

                      val searchFutures: List[Future[(String, TaxonomyDataHolder.SearchResult)]] = unmatchedParts.map { mp =>
                        val bricksetPart = bricksetMap.get(mp.coloredPart.partNumber).flatten
                        bricksetPart match {
                          case Some(bp) if bp.categories.isEmpty =>
                            taxonomyDataHolder.ask(ref => TaxonomyDataHolder.SearchByName(mp.coloredPart.name, ref))(searchTimeout, scheduler).collect {
                              case sr: TaxonomyDataHolder.SearchResult => (mp.coloredPart.partNumber, sr)
                            }
                          case _ =>
                            Future.successful((mp.coloredPart.partNumber, TaxonomyDataHolder.SearchResult(Nil)))
                        }
                      }

                      val allSearchFutures: Future[Seq[(String, TaxonomyDataHolder.SearchResult)]] = Future.sequence(searchFutures)

                      allSearchFutures.onComplete {
                        case scala.util.Success(searchResults) =>
                          val searchMap = searchResults.toMap
                          val finalMatchedParts = matchedPartsWithBrickset ++ unmatchedParts.map { mp =>
                            val bricksetPart = bricksetMap.get(mp.coloredPart.partNumber).flatten
                            bricksetPart match {
                              case Some(bp) if bp.categories.isEmpty =>
                                val searchResult = searchMap.getOrElse(mp.coloredPart.partNumber, TaxonomyDataHolder.SearchResult(Nil))
                                val (updatedPart, guessed) = inferCategories(bp, mp.coloredPart.partNumber, mp.coloredPart.name, searchResult.parts, matchedPartsWithBrickset, logger)
                                MatchedPart(mp.coloredPart, Some(updatedPart), guessed)
                              case Some(bp) =>
                                MatchedPart(mp.coloredPart, Some(bp), false)
                              case None =>
                                MatchedPart(mp.coloredPart, None, false)
                            }
                          }
                          val sortedParts = finalMatchedParts.sorted
                          replyTo ! ProcessedParts(sortedParts)
                        case scala.util.Failure(ex) =>
                          logger.error(s"Failed to search by name: ${ex.getMessage}")
                          val finalMatchedParts = matchedPartsWithBrickset ++ unmatchedParts.map { mp =>
                            val bricksetPart = bricksetMap.get(mp.coloredPart.partNumber).flatten
                            MatchedPart(mp.coloredPart, bricksetPart, false)
                          }
                          val sortedParts = finalMatchedParts.sorted
                          replyTo ! ProcessedParts(sortedParts)
                      }

                    case scala.util.Failure(ex) =>
                      logger.error(s"Failed to get Brickset data: ${ex.getMessage}")
                      val sortedParts = matchedPartsWithoutBrickset.sorted
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

  private def inferCategories(bricksetPart: LegoPart, coloredPartNumber: String, coloredPartName: String, searchResults: List[(LegoPart, Int)], matchedParts: List[MatchedPart], logger: org.slf4j.Logger): (LegoPart, Boolean) = {
    logger.debug(s"Fuzzy matching: partNumber=$coloredPartNumber, name=$coloredPartName")
    val bricksetWordsTokenized = PartNameIndex.tokenize(coloredPartName)
    logger.debug(s"  Tokenized: ${bricksetWordsTokenized.mkString(", ")}")
    logger.debug(s"  Top 5 fuzzy matches: ${searchResults.take(5).map { case (part, count) => s"${part.name} ($count)" }.mkString(", ")}")

    if (searchResults.isEmpty) {
      return (bricksetPart, false)
    }

    val bricksetWords = bricksetWordsTokenized.toSet

    val exactMatch = searchResults.find { case (part, _) =>
      val partWords = PartNameIndex.tokenize(part.name).toSet
      partWords.subsetOf(bricksetWords)
    }

    exactMatch match {
      case Some((matchedPart, _)) =>
        val newName = s"${matchedPart.name} (guessed)"
        val updatedPart = bricksetPart.copy(name = newName, categories = matchedPart.categories)
        (updatedPart, true)
      case None =>
        val top5 = searchResults.take(5)
        val commonPrefix = PartNameIndex.findCommonCategoryPrefix(top5.map(_._1))
        if (commonPrefix.nonEmpty) {
          val bestMatch = top5.find { case (part, _) =>
            part.categories.zip(commonPrefix).forall { case (cat, prefixCat) => cat == prefixCat }
          }
          val newName = bestMatch.map(p => s"${p._1.name} (guessed)").getOrElse(bricksetPart.name)
          val updatedPart = bricksetPart.copy(name = newName, categories = commonPrefix)
          (updatedPart, true)
        } else {
          val bricksetNameLower = bricksetPart.name.toLowerCase

          val prefixMatch = matchedParts
            .flatMap(_.legoPart)
            .map { legoPart =>
              val matchedPart = matchedParts.find(mp => mp.legoPart.contains(legoPart)).get
              (legoPart, matchedPart.coloredPart.name)
            }
            .filter { case (_, coloredPartName) =>
              bricksetNameLower.startsWith(coloredPartName.toLowerCase)
            }
            .sortBy { case (_, coloredPartName) => -coloredPartName.length }
            .headOption

          prefixMatch match {
            case Some((matchedLegoPart, _)) =>
              val newName = s"${matchedLegoPart.name} (guessed)"
              val updatedPart = bricksetPart.copy(name = newName, categories = matchedLegoPart.categories)
              (updatedPart, true)
            case None =>
              (bricksetPart, false)
          }
        }
    }
  }
}
