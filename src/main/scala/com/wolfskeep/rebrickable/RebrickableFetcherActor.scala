package com.wolfskeep.rebrickable

import java.nio.file.{Files, Paths, StandardCopyOption}

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpRequest
import akka.http.scaladsl.model.StatusCodes
import akka.stream.scaladsl.FileIO
import akka.stream.{Materializer, StreamTcpException}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Success, Failure}

object RebrickableFetcherActor {
  sealed trait Command
  case object FetchAll extends Command
  case class FetchResult(data: Data) extends Command
  case class FetchFailed(reason: Throwable) extends Command

  private case class DownloadResult(name: String, success: Boolean, message: String)
  private case class AllDownloadsComplete(results: List[DownloadResult]) extends Command
  private case class AllDownloadsFailed(ex: Throwable) extends Command

  private val BaseUrl = "https://cdn.rebrickable.com/media/downloads/"
  private val ZipFiles = List(
    "colors.csv.zip",
    "parts.csv.zip",
    "elements.csv.zip",
    "sets.csv.zip",
    "inventories.csv.zip",
    "inventory_parts.csv.zip"
  )

  def apply(dataActor: ActorRef[RebrickableDataActor.Command]): Behavior[Command] = Behaviors.setup { context =>
    implicit val system = context.system.classicSystem
    implicit val materializer: Materializer = Materializer(context)
    implicit val ec: ExecutionContext = context.executionContext

    def isFresh(name: String): Boolean = RebrickableBinaryCache.isFresh(name)
    def getAgeHours(name: String): Double = RebrickableBinaryCache.getAgeHours(name)
    def canRetry(name: String): Boolean = RebrickableBinaryCache.canRetry(name)
    def getRetryCount(name: String): Int = RebrickableBinaryCache.getRetryCount(name)
    def recordRetry(name: String): Int = RebrickableBinaryCache.recordRetry(name)
    def getCachePath(name: String): java.nio.file.Path = RebrickableBinaryCache.getCachePath(name)
    def readCache(name: String): scala.util.Try[Array[Byte]] = RebrickableBinaryCache.read(name)

    def downloadFile(fileName: String): Future[DownloadResult] = {
      if (isFresh(fileName)) {
        Future.successful(DownloadResult(fileName, true, f"skipped (cache fresh, ${getAgeHours(fileName)}%.1fh old)"))
      } else if (!canRetry(fileName)) {
        Future.successful(DownloadResult(fileName, false, s"retry exhausted (${getRetryCount(fileName)} attempts)"))
      } else {
        downloadWithRetry(fileName)
      }
    }

    def downloadWithRetry(fileName: String): Future[DownloadResult] = {
      val url = BaseUrl + fileName
      val cachePath = getCachePath(fileName)
      val tempPath = Paths.get(cachePath.toString + ".tmp")

      Http().singleRequest(HttpRequest(uri = url)).flatMap { response =>
        if (response.status == StatusCodes.OK) {
          response.entity.dataBytes
            .runWith(FileIO.toPath(tempPath))
            .flatMap { ioResult =>
              if (ioResult.status.isSuccess) {
                try {
                  Files.move(tempPath, cachePath, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE)

                  readCache(fileName) match {
                    case Success(_) =>
                      updateDataActor(fileName, dataActor)
                      Future.successful(DownloadResult(fileName, true, "downloaded"))
                    case Failure(_) =>
                      Files.deleteIfExists(cachePath)
                      val count = recordRetry(fileName)
                      if (canRetry(fileName)) {
                        downloadWithRetry(fileName)
                      } else {
                        Future.successful(DownloadResult(fileName, false, s"read failed after $count attempts"))
                      }
                  }
                } catch {
                  case ex: Exception =>
                    Files.deleteIfExists(tempPath)
                    Files.deleteIfExists(cachePath)
                    Future.successful(DownloadResult(fileName, false, s"move failed: ${ex.getMessage}"))
                }
              } else {
                Files.deleteIfExists(tempPath)
                Future.successful(DownloadResult(fileName, false, s"io failed: ${ioResult.status}"))
              }
            }
        } else {
          Files.deleteIfExists(tempPath)
          Future.successful(DownloadResult(fileName, false, s"http ${response.status}"))
        }
      }.recoverWith {
        case _: StreamTcpException =>
          Files.deleteIfExists(tempPath)
          val count = recordRetry(fileName)
          if (canRetry(fileName)) {
            downloadWithRetry(fileName)
          } else {
            Future.successful(DownloadResult(fileName, false, s"connection failed after $count attempts"))
          }
        case ex =>
          Files.deleteIfExists(tempPath)
          Future.successful(DownloadResult(fileName, false, s"error: ${ex.getMessage}"))
      }
    }

    Behaviors.receiveMessage[Command] {
      case FetchAll =>
        context.log.info("RebrickableFetcher: Starting fetch of {} files", ZipFiles.size)

        RebrickableBinaryCache.resetDailyRetries()

        val downloadFutures = ZipFiles.map(downloadFile)

        val allDownloads = Future.sequence(downloadFutures)

        context.pipeToSelf(allDownloads) {
          case Success(results) =>
            val failures = results.filter(!_.success)
            if (failures.nonEmpty) {
              context.log.warn("RebrickableFetcher: {} file(s) failed: {}",
                failures.size, failures.map(f => s"${f.name}: ${f.message}").mkString("; "))
            }
            AllDownloadsComplete(results)
          case Failure(ex) =>
            AllDownloadsFailed(ex)
        }

        Behaviors.same

      case AllDownloadsComplete(results) =>
        val successCount = results.count(_.success)
        context.log.info("RebrickableFetcher: {} of {} files processed successfully", successCount, ZipFiles.size)

        val data = Data.load()
        context.log.info(
          "RebrickableFetcher: Data loaded: {} parts, {} colors, {} elements, {} sets, {} inventories, {} inventoryParts",
          data.parts.size: Integer, data.colors.size: Integer, data.elements.size: Integer,
          data.sets.size: Integer, data.inventories.size: Integer, data.inventoryParts.size: Integer
        )

        dataActor ! RebrickableDataActor.SetColors(data.colors)
        dataActor ! RebrickableDataActor.SetParts(data.parts)
        dataActor ! RebrickableDataActor.SetElements(data.elements)
        dataActor ! RebrickableDataActor.SetSets(data.sets)
        dataActor ! RebrickableDataActor.SetInventories(data.inventories)
        dataActor ! RebrickableDataActor.SetInventoryParts(data.inventoryParts)

        Behaviors.same

      case AllDownloadsFailed(ex) =>
        context.log.error("RebrickableFetcher: All downloads failed: {}", ex.getMessage)
        Behaviors.same

      case FetchResult(_) | FetchFailed(_) =>
        Behaviors.same
    }
  }

  private def updateDataActor(fileName: String, dataActor: ActorRef[RebrickableDataActor.Command]): Unit = {
    fileName match {
      case "colors.csv.zip" =>
        Color.fromZip() match {
          case colors if colors.nonEmpty => dataActor ! RebrickableDataActor.SetColors(colors)
          case _ =>
        }
      case "parts.csv.zip" =>
        Part.fromZip() match {
          case parts if parts.nonEmpty => dataActor ! RebrickableDataActor.SetParts(parts)
          case _ =>
        }
      case "elements.csv.zip" =>
        Element.fromZip() match {
          case elements if elements.nonEmpty => dataActor ! RebrickableDataActor.SetElements(elements)
          case _ =>
        }
      case "sets.csv.zip" =>
        RebrickableSet.fromZip() match {
          case sets if sets.nonEmpty => dataActor ! RebrickableDataActor.SetSets(sets)
          case _ =>
        }
      case "inventories.csv.zip" =>
        Inventory.fromZip() match {
          case inventories if inventories.nonEmpty => dataActor ! RebrickableDataActor.SetInventories(inventories)
          case _ =>
        }
      case "inventory_parts.csv.zip" =>
        InventoryPart.fromZip() match {
          case inventoryParts if inventoryParts.nonEmpty => dataActor ! RebrickableDataActor.SetInventoryParts(inventoryParts)
          case _ =>
        }
    }
  }
}
