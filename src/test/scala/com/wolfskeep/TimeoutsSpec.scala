package com.wolfskeep

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._

class TimeoutsSpec extends AnyFlatSpec with Matchers {

  "Timeouts.web" should "order the web request budget: server > route >= data + process" in {
    Timeouts.web.httpServerRequestTimeout should be > Timeouts.web.routeRequest
    (Timeouts.web.dataAsk + Timeouts.web.processAsk) should be <= Timeouts.web.routeRequest
  }

  it should "expose positive image budgets" in {
    Timeouts.web.imageWait should be > Duration.Zero
    Timeouts.web.imageRetryAfter should be > Duration.Zero
    Timeouts.web.imageNegativeTtl should be > Duration.Zero
  }

  "Timeouts.Web" should "reject a server timeout that does not exceed the route budget" in {
    an[IllegalArgumentException] should be thrownBy {
      Timeouts.Web(
        httpServerRequestTimeout = 1.second,
        routeRequest = 2.seconds,
        dataAsk = 1.second,
        processAsk = 1.second,
        imageWait = 1.second,
        imageRetryAfter = 1.second,
        imageNegativeTtl = 1.second
      )
    }
  }

  it should "reject a route budget that does not cover its ask budgets" in {
    an[IllegalArgumentException] should be thrownBy {
      Timeouts.Web(
        httpServerRequestTimeout = 3.seconds,
        routeRequest = 2.seconds,
        dataAsk = 2.seconds,
        processAsk = 2.seconds,
        imageWait = 1.second,
        imageRetryAfter = 1.second,
        imageNegativeTtl = 1.second
      )
    }
  }

  it should "reject a non-positive image negative TTL" in {
    an[IllegalArgumentException] should be thrownBy {
      Timeouts.Web(
        httpServerRequestTimeout = 2.seconds,
        routeRequest = 1.second,
        dataAsk = 400.millis,
        processAsk = 600.millis,
        imageWait = 500.millis,
        imageRetryAfter = 50.millis,
        imageNegativeTtl = Duration.Zero
      )
    }
  }
}
