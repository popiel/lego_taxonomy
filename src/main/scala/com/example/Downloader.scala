package com.example

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors

import scala.concurrent.Future
import scala.concurrent.duration._

import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpRequest
import akka.http.scaladsl.unmarshalling.Unmarshal

object Downloader {
  // commands the actor understands
  sealed trait Command
  final case class Fetch(url: String, replyTo: ActorRef[Response]) extends Command

  // internal messages after fetching
  private final case class WrappedResult(url: String, content: String, replyTo: ActorRef[Response]) extends Command
  private final case class WrappedFailure(url: String, cause: Throwable, replyTo: ActorRef[Response]) extends Command

  // responses we send back
  sealed trait Response
  final case class Downloaded(url: String, content: String) extends Response
  final case class Failed(url: String, reason: String) extends Response

  def apply(): Behavior[Command] = Behaviors.setup { context =>
    implicit val ec = context.executionContext

    Behaviors.receiveMessage {
      case Fetch(url, replyTo) =>
        // use akka-http to perform the request
        implicit val system = context.system
        // timeout for entity toStrict
        val timeout = 5.seconds
        val responseFuture = Http(context.system.classicSystem)
          .singleRequest(HttpRequest(uri = url))
          .flatMap(res => Unmarshal(res.entity).to[String])

        context.pipeToSelf(responseFuture) {
          case scala.util.Success(content) => WrappedResult(url, content, replyTo)
          case scala.util.Failure(ex)      => WrappedFailure(url, ex, replyTo)
        }
        Behaviors.same

      case WrappedResult(url, content, replyTo) =>
        replyTo ! Downloaded(url, content)
        Behaviors.same

      case WrappedFailure(url, cause, replyTo) =>
        replyTo ! Failed(url, cause.getMessage)
        Behaviors.same
    }
  }
}
