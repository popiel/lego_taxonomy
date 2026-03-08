package com.example

//#imports
import akka.actor.typed.ActorRef
import akka.actor.typed.ActorSystem
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
//#imports


//#hello-world-main
object HelloWorldMain {

  //#hello-world-main
  def main(args: Array[String]): Unit = {
    val system: ActorSystem[TaxonomyFetcher.Command] = ActorSystem(TaxonomyFetcher(), "taxonomy-fetcher-system")
    system ! TaxonomyFetcher.GetTaxonomy

    Thread.sleep(10000) // wait for the fetch to complete before shutting down
    system.terminate()
  }
  //#hello-world-main
}
//#hello-world-main
