package com.wolfskeep

import java.util.concurrent.TimeUnit

import com.typesafe.config.{Config, ConfigFactory}

import scala.concurrent.duration._

object Timeouts {
  private def duration(c: Config, path: String): FiniteDuration =
    FiniteDuration(c.getDuration(path).toNanos, TimeUnit.NANOSECONDS)

  final case class Web(
    httpServerRequestTimeout: FiniteDuration,
    routeRequest: FiniteDuration,
    dataAsk: FiniteDuration,
    processAsk: FiniteDuration,
    imageWait: FiniteDuration,
    imageRetryAfter: FiniteDuration,
    imageNegativeTtl: FiniteDuration
  ) {
    require(
      httpServerRequestTimeout > routeRequest,
      s"akka.http.server.request-timeout ($httpServerRequestTimeout) must exceed the route budget ($routeRequest)"
    )
    require(
      routeRequest >= dataAsk + processAsk,
      s"route budget ($routeRequest) must cover the data ($dataAsk) and process ($processAsk) asks"
    )
    require(imageWait > Duration.Zero, s"imageWait ($imageWait) must be positive")
    require(imageRetryAfter > Duration.Zero, s"imageRetryAfter ($imageRetryAfter) must be positive")
    require(
      imageNegativeTtl > Duration.Zero,
      s"imageNegativeTtl ($imageNegativeTtl) must be positive"
    )
  }

  object Web {
    def fromConfig(root: Config = ConfigFactory.load()): Web = {
      val config = root.getConfig("lego-taxonomy.web")
      Web(
        httpServerRequestTimeout = duration(root, "akka.http.server.request-timeout"),
        routeRequest = duration(config, "route-request"),
        dataAsk = duration(config, "data-ask"),
        processAsk = duration(config, "process-ask"),
        imageWait = duration(config, "image-wait"),
        imageRetryAfter = duration(config, "image-retry-after"),
        imageNegativeTtl = duration(config, "image-negative-ttl")
      )
    }
  }

  val web: Web = Web.fromConfig()

  final case class Service(
    lookupAsk: FiniteDuration,
    rebrickableAsk: FiniteDuration,
    bricksetAsk: FiniteDuration,
    imageAsk: FiniteDuration
  ) {
    require(lookupAsk > Duration.Zero, s"lookup-ask ($lookupAsk) must be positive")
    require(rebrickableAsk > Duration.Zero, s"rebrickable-ask ($rebrickableAsk) must be positive")
    require(bricksetAsk > Duration.Zero, s"brickset-ask ($bricksetAsk) must be positive")
    require(imageAsk > Duration.Zero, s"image-ask ($imageAsk) must be positive")
  }

  object Service {
    def fromConfig(root: Config = ConfigFactory.load()): Service = {
      val config = root.getConfig("lego-taxonomy.service")
      Service(
        lookupAsk = duration(config, "lookup-ask"),
        rebrickableAsk = duration(config, "rebrickable-ask"),
        bricksetAsk = duration(config, "brickset-ask"),
        imageAsk = duration(config, "image-ask")
      )
    }
  }

  val service: Service = Service.fromConfig()

  final case class Scheduled(
    rebrickableFetch: FiniteDuration,
    batchProbe: FiniteDuration
  ) {
    require(rebrickableFetch > Duration.Zero, s"rebrickable-fetch ($rebrickableFetch) must be positive")
    require(batchProbe > Duration.Zero, s"batch-probe ($batchProbe) must be positive")
  }

  object Scheduled {
    def fromConfig(root: Config = ConfigFactory.load()): Scheduled = {
      val config = root.getConfig("lego-taxonomy.scheduled")
      Scheduled(
        rebrickableFetch = duration(config, "rebrickable-fetch"),
        batchProbe = duration(config, "batch-probe")
      )
    }
  }

  val scheduled: Scheduled = Scheduled.fromConfig()
}
