package com.example

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors

import scala.concurrent.Future
import scala.concurrent.duration._

import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpRequest
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.DateTime
import akka.http.scaladsl.model.headers._
import akka.http.scaladsl.unmarshalling.Unmarshal

object Downloader {
  // commands the actor understands
  sealed trait Command
  /**
   * @param url the address to fetch
   * @param replyTo actor to reply with a [[Response]]
   * @param since optional If-Modified-Since header value
   */
  final case class Fetch(url: String, replyTo: ActorRef[Response], since: Option[DateTime] = None) extends Command

  // internal messages after fetching
  private final case class WrappedResult(url: String, content: String, replyTo: ActorRef[Response]) extends Command
  private final case class WrappedNotChanged(url: String, replyTo: ActorRef[Response]) extends Command
  private final case class WrappedFailure(url: String, cause: Throwable, replyTo: ActorRef[Response]) extends Command

  // responses we send back
  sealed trait Response
  final case class Downloaded(url: String, content: String) extends Response
  final case class NotChanged(url: String) extends Response
  final case class Failed(url: String, reason: String) extends Response

  def apply(): Behavior[Command] = Behaviors.setup { context =>
    implicit val ec = context.executionContext

    Behaviors.receiveMessage {
      case Fetch(url, replyTo, since) =>
        // use akka-http to perform the request, attaching header if requested
        implicit val system = context.system
        val request = since match {
          case Some(date) => HttpRequest(uri = url).withHeaders(`If-Modified-Since`(date))
          case None       => HttpRequest(uri = url)
        }
        // timeout for entity toStrict
        val timeout = 5.seconds
        val responseFuture = Http(context.system.classicSystem)
          .singleRequest(request)
          .flatMap { res =>
            if (res.status == StatusCodes.NotModified) {
              Future.successful(Left(()))
            } else if (res.status.isSuccess()) {
              Unmarshal(res.entity).to[String].map(Right(_))
            } else {
              Future.failed(new RuntimeException(s"HTTP ${res.status}"))
            }
          }

        context.pipeToSelf(responseFuture) {
          case scala.util.Success(Right(content)) => WrappedResult(url, content, replyTo)
          case scala.util.Success(Left(_))        => WrappedNotChanged(url, replyTo)
          case scala.util.Failure(ex)              => WrappedFailure(url, ex, replyTo)
        }
        Behaviors.same

      case WrappedResult(url, content, replyTo) =>
        replyTo ! Downloaded(url, content)
        Behaviors.same

      case WrappedNotChanged(url, replyTo) =>
        replyTo ! NotChanged(url)
        Behaviors.same

      case WrappedFailure(url, cause, replyTo) =>
        replyTo ! Failed(url, cause.getMessage)
        Behaviors.same
    }
  }
}
