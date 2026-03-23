package com.wolfskeep.rebrickable

import java.util.zip.ZipFile

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.model.{HttpRequest, HttpResponse}
import akka.http.scaladsl.Http
import akka.stream.scaladsl.Sink
import akka.stream.{Materializer, ActorMaterializer}
import akka.util.ByteString

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.{FiniteDuration, DurationInt}

class LDrawImageFetcher(implicit val system: ActorSystem[_]) {
  private val BaseUrl = "https://cdn.rebrickable.com/media/downloads/ldraw"
  private val Timeout: FiniteDuration = 5.minutes

  def ensureDownloaded(colorId: Int)(implicit ec: ExecutionContext): Boolean = {
    if (RebrickableBinaryCache.isLDrawFresh(colorId)) {
      return true
    }

    val name = s"parts_$colorId.zip"
    var retries = 0
    var success = false

    while (retries < RebrickableBinaryCache.MaxRetries && !success) {
      if (!RebrickableBinaryCache.canRetry(name)) {
        retries = RebrickableBinaryCache.MaxRetries
      } else {
        success = downloadColorZip(colorId)
        if (!success) {
          retries += 1
          val count = RebrickableBinaryCache.recordRetry(name)
          if (retries < RebrickableBinaryCache.MaxRetries) {
            Thread.sleep(1000L * retries * retries)
          }
        }
      }
    }

    success
  }

  private def downloadColorZip(colorId: Int)(implicit ec: ExecutionContext): Boolean = {
    val url = s"$BaseUrl/parts_$colorId.zip"
    try {
      implicit val classicSystem = system.classicSystem
      implicit val materializer: Materializer = ActorMaterializer()
      
      val futureResponse: Future[HttpResponse] = Http(classicSystem)
        .singleRequest(HttpRequest(uri = url))

      val result = futureResponse.flatMap { resp =>
        if (resp.status.isSuccess()) {
          resp.entity.dataBytes.runWith(Sink.reduce[ByteString](_ ++ _)).map { byteString =>
            val bytes = byteString.toArray
            RebrickableBinaryCache.writeLDraw(colorId, bytes)
            true
          }
        } else {
          Future.successful(false)
        }
      }

      scala.concurrent.Await.result(result, Timeout)
    } catch {
      case _: Exception => false
    }
  }

  def prefetchAll(colorIds: Set[Int])(implicit ec: ExecutionContext): Unit = {
    colorIds.foreach { colorId =>
      ensureDownloaded(colorId)
    }
  }

  def getImageFromZip(colorId: Int, partNumber: String): Option[Array[Byte]] = {
    val zipPath = RebrickableBinaryCache.getLDrawCachePath(colorId)
    if (!java.nio.file.Files.exists(zipPath)) {
      return None
    }

    val zipFile = new ZipFile(zipPath.toFile)
    try {
      val entryName = s"$partNumber.png"
      val entry = zipFile.getEntry(entryName)
      if (entry == null) {
        None
      } else {
        val stream = zipFile.getInputStream(entry)
        try {
          val bytes = java.nio.file.Files.readAllBytes(java.nio.file.Paths.get(entry.getName))
          Some(bytes)
        } catch {
          case _: Exception => None
        } finally {
          stream.close()
        }
      }
    } catch {
      case _: Exception => None
    } finally {
      zipFile.close()
    }
  }

  def hasImageInZip(colorId: Int, partNumber: String): Boolean = {
    val zipPath = RebrickableBinaryCache.getLDrawCachePath(colorId)
    if (!java.nio.file.Files.exists(zipPath)) {
      return false
    }

    var zipFile: ZipFile = null
    try {
      zipFile = new ZipFile(zipPath.toFile)
      val entryName = s"$partNumber.png"
      zipFile.getEntry(entryName) != null
    } catch {
      case _: Exception => false
    } finally {
      if (zipFile != null) {
        zipFile.close()
      }
    }
  }
}
