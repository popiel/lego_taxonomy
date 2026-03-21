package com.wolfskeep.rebrickable

import java.nio.file.{Files, Paths, StandardOpenOption}
import java.time.LocalDate
import java.time.temporal.ChronoUnit

import scala.collection.mutable
import scala.util.Try

object RebrickableBinaryCache {
  private val CacheDir = Paths.get(".cache", "rebrickable")
  private[rebrickable] val FreshnessThresholdHours = 22.5
  private[rebrickable] val MaxRetries = 3

  private val retryCounts: mutable.Map[String, (LocalDate, Int)] = mutable.Map()

  def ensureCacheDir(): Unit = {
    if (!Files.exists(CacheDir)) {
      Files.createDirectories(CacheDir)
    }
  }

  def isFresh(name: String): Boolean = {
    val path = CacheDir.resolve(name)
    if (!Files.exists(path)) {
      return false
    }
    val lastModified = Files.getLastModifiedTime(path).toMillis
    val ageHours = (System.currentTimeMillis() - lastModified).toDouble / (1000 * 60 * 60)
    ageHours < FreshnessThresholdHours
  }

  def getAgeHours(name: String): Double = {
    val path = CacheDir.resolve(name)
    if (!Files.exists(path)) {
      return Double.MaxValue
    }
    val lastModified = Files.getLastModifiedTime(path).toMillis
    (System.currentTimeMillis() - lastModified).toDouble / (1000 * 60 * 60)
  }

  def write(name: String, bytes: Array[Byte]): Unit = {
    ensureCacheDir()
    val path = CacheDir.resolve(name)
    Files.write(path, bytes, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)
  }

  def read(name: String): Try[Array[Byte]] = {
    Try {
      Files.readAllBytes(CacheDir.resolve(name))
    }
  }

  def delete(name: String): Unit = {
    val path = CacheDir.resolve(name)
    if (Files.exists(path)) {
      Files.delete(path)
    }
  }

  def canRetry(name: String): Boolean = {
    val today = LocalDate.now()
    retryCounts.get(name) match {
      case Some((date, count)) =>
        if (date == today) {
          count < MaxRetries
        } else {
          retryCounts(name) = (today, 0)
          true
        }
      case None =>
        retryCounts(name) = (today, 0)
        true
    }
  }

  def recordRetry(name: String): Int = {
    val today = LocalDate.now()
    val count = retryCounts.get(name) match {
      case Some((date, c)) if date == today => c + 1
      case _ => 1
    }
    retryCounts(name) = (today, count)
    count
  }

  def resetDailyRetries(): Unit = {
    val today = LocalDate.now()
    retryCounts.retain { case (_, (date, _)) => date == today }
  }

  def getRetryCount(name: String): Int = {
    val today = LocalDate.now()
    retryCounts.get(name) match {
      case Some((date, count)) if date == today => count
      case _ => 0
    }
  }

  def getCachePath(name: String): java.nio.file.Path = {
    CacheDir.resolve(name)
  }
}
