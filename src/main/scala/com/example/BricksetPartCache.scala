package com.example

import akka.actor.typed.{ActorRef, Behavior, Scheduler}
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.AskPattern._
import akka.util.Timeout
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext

object BricksetPartCache {
  sealed trait Command
  case class GetPart(partNumber: String, replyTo: ActorRef[Option[LegoPart]]) extends Command

  private case class FetchedPart(partNumber: String, part: Option[LegoPart], replyTo: ActorRef[Option[LegoPart]]) extends Command

  def apply(downloader: ActorRef[CachedDownloader.Command]): Behavior[Command] = {
    Behaviors.setup { context =>
      implicit val ec: ExecutionContext = context.executionContext
      implicit val scheduler: Scheduler = context.system.scheduler
      implicit val timeout: Timeout = 30.seconds

      cached(Map.empty, downloader)
    }
  }

  private def cached(parts: Map[String, LegoPart], downloader: ActorRef[CachedDownloader.Command]): Behavior[Command] = {
    Behaviors.receive { (context, message) =>
      implicit val ec: ExecutionContext = context.executionContext
      implicit val scheduler: Scheduler = context.system.scheduler
      implicit val timeout: Timeout = 30.seconds

      message match {
        case GetPart(partNumber, replyTo) =>
          parts.get(partNumber) match {
            case Some(part) =>
              replyTo ! Some(part)
              Behaviors.same
            case None =>
              val fetchResult = BricksetPartFetcher.fetchPartDetails(downloader, partNumber)
              context.pipeToSelf(fetchResult) {
                case scala.util.Success(fetchedPart: Option[LegoPart]) =>
                  FetchedPart(partNumber, fetchedPart, replyTo)
                case scala.util.Failure(ex) =>
                  FetchedPart(partNumber, None, replyTo)
              }
              cached(parts, downloader)
          }

        case FetchedPart(partNumber, maybePart, replyTo) =>
          replyTo ! maybePart
          maybePart match {
            case Some(part) =>
              val newParts = parts + (partNumber -> part)
              cached(newParts, downloader)
            case None =>
              Behaviors.same
          }
      }
    }
  }
}
