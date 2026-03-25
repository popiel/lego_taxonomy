package com.wolfskeep

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpRequest
import akka.http.scaladsl.model.StatusCode
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.DateTime
import akka.http.scaladsl.model.HttpHeader
import akka.http.scaladsl.model.headers.{HttpCookie, HttpCookiePair, `Set-Cookie`, Location, Cookie, `If-Modified-Since`}
import akka.http.scaladsl.unmarshalling.Unmarshal

import scala.concurrent.Future
import scala.concurrent.duration._
import java.net.URI

object Downloader {
  sealed trait Command
  final case class Fetch(url: String, replyTo: ActorRef[Response], since: Option[DateTime] = None) extends Command

  private final case class WrappedResult(url: String, finalUrl: String, content: String, replyTo: ActorRef[Response], newCookies: Seq[HttpCookie]) extends Command
  private final case class WrappedNotChanged(url: String, replyTo: ActorRef[Response]) extends Command
  private final case class WrappedFailure(url: String, cause: Throwable, replyTo: ActorRef[Response]) extends Command
  private final case object NoOp extends Command

  sealed trait Response
  final case class Downloaded(url: String, content: String) extends Response
  final case class NotChanged(url: String) extends Response
  final case class Failed(url: String, reason: String) extends Response
  final case class TooManyRequests(url: String) extends Response

  private case class State(cookies: Map[String, Seq[HttpCookie]])

  def apply(retryInterval: FiniteDuration = 60.seconds, retryOn429: Boolean = true): Behavior[Command] = Behaviors.withTimers[Command] { timers =>
    Behaviors.setup { context =>
      context.log.info(s"===> cookie-parsing-mode = ${context.system.settings.config.getString("akka.http.parsing.cookie-parsing-mode")}")

      implicit val ec = context.executionContext
      val retryIntervalValue = retryInterval
      val log = context.log

      def handle429(url: String, replyTo: ActorRef[Response], since: Option[DateTime]): Command = {
        if (retryOn429) {
          context.log.warn(s"Received 429, waiting $retryIntervalValue before retrying $url")
          val retryMsg: Command = Fetch(url, replyTo, since)
          timers.startSingleTimer(s"retry-$url", retryMsg, retryIntervalValue)
          NoOp
        } else {
          replyTo ! TooManyRequests(url)
          NoOp
        }
      }

      def getDomain(url: String): String = {
        new URI(url).getHost
      }

      def getCookiesForDomain(state: State, domain: String): Seq[HttpCookie] = {
        state.cookies.getOrElse(domain, Nil)
      }

      def buildCookiePairs(cookies: Seq[HttpCookie]): Seq[HttpCookiePair] = {
        cookies.map(c => HttpCookiePair(c.name, c.value))
      }

      def extractCookiesFromHeaders(headers: Seq[HttpHeader], domain: String): Seq[HttpCookie] = {
        headers.collect {
          case `Set-Cookie`(cookie) => cookie
        }
      }

      def isRedirect(status: StatusCode): Boolean = {
        status match {
          case StatusCodes.MovedPermanently => true
          case StatusCodes.Found => true
          case StatusCodes.TemporaryRedirect => true
          case StatusCodes.PermanentRedirect => true
          case _ => false
        }
      }

      def fetchUrl(url: String, replyTo: ActorRef[Response], since: Option[DateTime], state: State): Unit = {
        implicit val system = context.system
        val domain = getDomain(url)
        val cookies = getCookiesForDomain(state, domain)

        val baseRequest = HttpRequest(uri = url)
        val request = cookies match {
          case Nil => baseRequest
          case cs =>
            val pairs = buildCookiePairs(cs)
            baseRequest.withHeaders(Cookie(pairs.head, pairs.tail: _*))
        }
        val requestWithModified = since match {
          case Some(date) => request.withHeaders(`If-Modified-Since`(date))
          case None => request
        }

        if (domain.toLowerCase.contains("brickset")) {
          log.info(s"Brickset request: $url")
          // requestWithModified.headers.foreach(h => log.debug(s"  Request header: ${h.name} = ${h.value}"))
        }

        val responseFuture = Http(context.system.classicSystem)
          .singleRequest(requestWithModified)
          .flatMap { res =>
            if (domain.toLowerCase.contains("brickset")) {
              log.info(s"Brickset response: ${res.status}")
              // res.headers.foreach(h => log.debug(s"  Response header: ${h.name} = ${h.value}"))
            }
            if (res.status == StatusCodes.NotModified) {
              Future.successful(Left((None, None, None)))
            } else if (res.status == StatusCodes.TooManyRequests) {
              Future.failed(new TooManyRequestsException)
            } else if (isRedirect(res.status)) {
              val locationHeader = res.header[Location]
              locationHeader match {
                case Some(location) =>
                  val redirectUrl = resolveRedirect(url, location.uri.toString)
                  Future.successful(Left((Some(redirectUrl), Some(res.headers), None)))
                case None =>
                  Future.failed(new RuntimeException(s"Redirect without Location header"))
              }
            } else if (res.status.isSuccess()) {
              Unmarshal(res.entity).to[String].map(content => Left((None, Some(res.headers), Some(content))))
            } else {
              Future.failed(new RuntimeException(s"HTTP ${res.status}"))
            }
          }

        context.pipeToSelf(responseFuture) {
          case scala.util.Success(Left((None, _, None))) =>
            WrappedNotChanged(url, replyTo)
          case scala.util.Success(Left((redirectUrl, headers, content))) =>
            redirectUrl match {
              case Some(newUrl) if newUrl != url =>
                fetchUrl(newUrl, replyTo, since, state)
                NoOp
              case _ =>
                val finalUrl = redirectUrl.getOrElse(url)
                val newCookies = headers.map(h => extractCookiesFromHeaders(h, domain)).getOrElse(Nil)
                WrappedResult(url, finalUrl, content.getOrElse(""), replyTo, newCookies)
            }
          case scala.util.Failure(ex: TooManyRequestsException) => handle429(url, replyTo, since)
          case scala.util.Failure(ex) => WrappedFailure(url, ex, replyTo)
        }
      }

      def resolveRedirect(baseUrl: String, location: String): String = {
        val baseUri = new URI(baseUrl)
        val resolvedUri = baseUri.resolve(location)
        resolvedUri.toString
      }

      def running(state: State): Behavior[Command] = {
        Behaviors.receive { (context, message) =>
          message match {
            case Fetch(url, replyTo, since) =>
              fetchUrl(url, replyTo, since, state)
              running(state)

            case WrappedResult(originalUrl, finalUrl, content, replyTo, newCookies) =>
              val domain = getDomain(originalUrl)
              val updatedCookies = if (newCookies.nonEmpty) {
                val existing = state.cookies.getOrElse(domain, Nil)
                state.cookies + (domain -> (newCookies ++ existing))
              } else {
                state.cookies
              }
              replyTo ! Downloaded(originalUrl, content)
              running(State(updatedCookies))

            case WrappedNotChanged(url, replyTo) =>
              replyTo ! NotChanged(url)
              running(state)

            case WrappedFailure(url, cause, replyTo) =>
              replyTo ! Failed(url, cause.getMessage)
              running(state)

            case NoOp =>
              running(state)
          }
        }
      }

      running(State(Map.empty))
    }
  }

  private class TooManyRequestsException extends RuntimeException("HTTP 429")
}
