package com.example

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
                    bricksetPartCache.ask(BricksetPartCache.GetPart(mp.coloredPart.partNumber, _))(bricksetTimeout, scheduler).map { bricksetPart =>
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
                                val (updatedPart, guessed) = inferCategories(bp, mp.coloredPart.name, searchResult.parts, matchedPartsWithBrickset)
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

  private def inferCategories(bricksetPart: LegoPart, coloredPartName: String, searchResults: List[(LegoPart, Int)], matchedParts: List[MatchedPart]): (LegoPart, Boolean) = {
    if (searchResults.isEmpty) {
      return (bricksetPart, false)
    }

    val bricksetWords = PartNameIndex.tokenize(coloredPartName).toSet

    val exactMatch = searchResults.find { case (part, _) =>
      val partWords = PartNameIndex.tokenize(part.name).toSet
      partWords.subsetOf(bricksetWords)
    }

    exactMatch match {
      case Some((matchedPart, _)) =>
        val updatedPart = bricksetPart.copy(categories = matchedPart.categories)
        (updatedPart, true)
      case None =>
        val top5 = searchResults.take(5)
        val commonPrefix = findCommonCategoryPrefix(top5.map(_._1))
        if (commonPrefix.nonEmpty) {
          val updatedPart = bricksetPart.copy(categories = commonPrefix)
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
              val updatedPart = bricksetPart.copy(categories = matchedLegoPart.categories)
              (updatedPart, true)
            case None =>
              (bricksetPart, false)
          }
        }
    }
  }

  private def findCommonCategoryPrefix(parts: List[LegoPart]): List[Category] = {
    if (parts.isEmpty) return Nil

    val hierarchies = parts.map(_.categories)
    if (hierarchies.exists(_.isEmpty)) return Nil

    val minLength = hierarchies.map(_.length).min
    if (minLength == 0) return Nil

    val commonPrefix = (0 until minLength).collect { i =>
      val categoryAtPosition = hierarchies.map(_.apply(i))
      if (categoryAtPosition.forall(_ == categoryAtPosition.head)) {
        Some(categoryAtPosition.head)
      } else {
        None
      }
    }.takeWhile(_.isDefined).flatten

    commonPrefix.toList
  }
}
