package com.wolfskeep

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors

import spray.json._
import spray.json.DefaultJsonProtocol._

import scala.io.Source
import java.io.{File, PrintWriter}
import scala.util.{Try, Success, Failure}
import java.net.URLEncoder
import java.nio.charset.StandardCharsets

object DiskCache {
  // JSON formats
  implicit val entryFormat: RootJsonFormat[CacheEntry] = jsonFormat3(CacheEntry)

  case class CacheEntry(key: String, value: String, insertedAt: Long)

  // commands
  sealed trait Command
  final case class Insert(key: String, value: String) extends Command
  final case class Fetch(key: String, replyTo: ActorRef[Response]) extends Command

  // responses
  sealed trait Response
  final case class FetchResult(key: String, value: String, insertedAt: Long) extends Response
  final case class NotFound(key: String) extends Response

  private val cacheDir = new File(".cache")

  private def keyToFilename(key: String): String = {
    URLEncoder.encode(key, StandardCharsets.UTF_8.toString)
  }

  private def getCacheEntryFile(key: String): File = {
    new File(cacheDir, keyToFilename(key) + ".json")
  }

  def apply(): Behavior[Command] = Behaviors.setup { context =>
    // load cache from disk
    val initialCache = loadCache(context.log)
    context.log.info(s"Loaded cache with ${initialCache.size} entries")
    running(initialCache)
  }

  private def running(cache: Map[String, CacheEntry]): Behavior[Command] = Behaviors.receive { (context, message) =>
    message match {
      case Insert(key, value) =>
        val now = System.currentTimeMillis()
        val entry = CacheEntry(key, value, now)
        val newCache = cache + (key -> entry)
        saveCache(context.log, entry)
        running(newCache)

      case Fetch(key, replyTo) =>
        cache.get(key) match {
          case Some(entry) => replyTo ! FetchResult(key, entry.value, entry.insertedAt)
          case None          => replyTo ! NotFound(key)
        }
        Behaviors.same
    }
  }

  private def loadCache(log: org.slf4j.Logger): Map[String, CacheEntry] = {
    if (!cacheDir.exists()) {
      Map.empty
    } else {
      cacheDir.listFiles() match {
        case null => Map.empty
        case files =>
          files
            .filter(_.getName.endsWith(".json"))
            .flatMap { file =>
              Try {
                val source = Source.fromFile(file)
                try {
                  val content = source.mkString
                  val entry = content.parseJson.convertTo[CacheEntry]
                  entry.key -> entry
                } finally source.close()
              } match {
                case Success(pair) => Some(pair)
                case Failure(ex) =>
                  log.warn(s"Failed to load ${file.getName}: ${ex.getMessage}")
                  None
              }
            }
            .toMap
      }
    }
  }

  private def saveCache(log: org.slf4j.Logger, entry: CacheEntry): Unit = {
    Try {
      if (!cacheDir.exists()) {
        cacheDir.mkdirs()
      }
      val file = getCacheEntryFile(entry.key)
      val json = entry.toJson.prettyPrint
      val writer = new PrintWriter(file)
      writer.write(json)
      writer.close()
    } match {
      case Failure(ex) => log.warn(s"Failed to save cache for ${entry.key}: ${ex.getMessage}")
      case _ =>
    }
  }
}
