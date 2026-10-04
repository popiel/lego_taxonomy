package com.wolfskeep

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._

class TaxonomySchedulerSpec extends ScalaTestWithActorTestKit with AnyWordSpecLike with Matchers {

  "TaxonomyScheduler" should {

    "relay the fetch request and leave taxonomy publication to the fetcher" in {
      val fetcherProbe = createTestProbe[TaxonomyFetcher.Command]()
      val holderProbe = createTestProbe[TaxonomyHolder.Command]()
      val scheduler = spawn(TaxonomyScheduler(fetcherProbe.ref, holderProbe.ref))

      scheduler ! TaxonomyScheduler.FetchTaxonomy
      val get = fetcherProbe.expectMessageType[TaxonomyFetcher.GetTaxonomy](3.seconds)

      get.replyTo ! TaxonomyFetcher.AugmentationComplete

      holderProbe.expectNoMessage(300.millis)
    }

    "stay available for the next fetch after a cycle completes" in {
      val fetcherProbe = createTestProbe[TaxonomyFetcher.Command]()
      val holderProbe = createTestProbe[TaxonomyHolder.Command]()
      val scheduler = spawn(TaxonomyScheduler(fetcherProbe.ref, holderProbe.ref))

      scheduler ! TaxonomyScheduler.FetchTaxonomy
      fetcherProbe.expectMessageType[TaxonomyFetcher.GetTaxonomy](3.seconds).replyTo ! TaxonomyFetcher.AugmentationComplete

      scheduler ! TaxonomyScheduler.FetchTaxonomy
      fetcherProbe.expectMessageType[TaxonomyFetcher.GetTaxonomy](3.seconds)
    }

    "handle a failed fetch cycle and stay available for the next fetch" in {
      val fetcherProbe = createTestProbe[TaxonomyFetcher.Command]()
      val holderProbe = createTestProbe[TaxonomyHolder.Command]()
      val scheduler = spawn(TaxonomyScheduler(fetcherProbe.ref, holderProbe.ref))

      scheduler ! TaxonomyScheduler.FetchTaxonomy
      fetcherProbe.expectMessageType[TaxonomyFetcher.GetTaxonomy](3.seconds).replyTo ! TaxonomyFetcher.Failed(new RuntimeException("test failure"))

      holderProbe.expectNoMessage(300.millis)

      scheduler ! TaxonomyScheduler.FetchTaxonomy
      fetcherProbe.expectMessageType[TaxonomyFetcher.GetTaxonomy](3.seconds)
    }
    "retry the fetch request when a cycle never completes" in {
      val fetcherProbe = createTestProbe[TaxonomyFetcher.Command]()
      val holderProbe = createTestProbe[TaxonomyHolder.Command]()
      val scheduler = spawn(TaxonomyScheduler(fetcherProbe.ref, holderProbe.ref, cycleTimeout = 200.millis))

      scheduler ! TaxonomyScheduler.FetchTaxonomy
      fetcherProbe.expectMessageType[TaxonomyFetcher.GetTaxonomy](3.seconds)

      // the fetcher never replies; the watchdog re-issues the fetch request
      fetcherProbe.expectMessageType[TaxonomyFetcher.GetTaxonomy](2.seconds)
    }
  }
}
