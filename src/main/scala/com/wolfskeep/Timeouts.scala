package com.wolfskeep

import java.util.concurrent.TimeUnit

import com.typesafe.config.{Config, ConfigFactory}

import scala.concurrent.duration._

object Timeouts {
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
      def duration(c: Config, path: String): FiniteDuration =
        FiniteDuration(c.getDuration(path).toNanos, TimeUnit.NANOSECONDS)
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
}
