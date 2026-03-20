package com.wolfskeep

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.ActorContext
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{HttpRequest, StatusCode}
import akka.http.scaladsl.model.headers.{Accept, RawHeader}
import akka.http.scaladsl.model.MediaTypes
import akka.http.scaladsl.unmarshalling.Unmarshal
import spray.json._
import spray.json.DefaultJsonProtocol._
import scala.concurrent.Future
import scala.util.{Try, Success, Failure}
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import java.util.Base64
import java.net.URLEncoder.encode
import java.net.URI
import scala.concurrent.ExecutionContext

object BricklinkActor {
  case class BricklinkCredentials(
    consumerKey: String,
    consumerSecret: String,
    tokenValue: String,
    tokenSecret: String
  )

  case class BricklinkItem(
    no: String,
    name: String,
    itemType: String,
    categoryId: Int,
    imageUrl: Option[String] = None,
    thumbnailUrl: Option[String] = None,
    weight: Option[String] = None,
    dimX: Option[String] = None,
    dimY: Option[String] = None,
    dimZ: Option[String] = None,
    yearReleased: Option[Int] = None,
    description: Option[String] = None,
    isObsolete: Boolean = false
  )

  implicit val bricklinkItemFormat: RootJsonFormat[BricklinkItem] = new RootJsonFormat[BricklinkItem] {
    def write(item: BricklinkItem): JsValue = JsObject(
      "no" -> JsString(item.no),
      "name" -> JsString(item.name),
      "type" -> JsString(item.itemType),
      "category_id" -> JsNumber(item.categoryId),
      "image_url" -> item.imageUrl.map(JsString).getOrElse(JsNull),
      "thumbnail_url" -> item.thumbnailUrl.map(JsString).getOrElse(JsNull),
      "weight" -> item.weight.map(JsString).getOrElse(JsNull),
      "dim_x" -> item.dimX.map(JsString).getOrElse(JsNull),
      "dim_y" -> item.dimY.map(JsString).getOrElse(JsNull),
      "dim_z" -> item.dimZ.map(JsString).getOrElse(JsNull),
      "year_released" -> item.yearReleased.map(JsNumber(_)).getOrElse(JsNull),
      "description" -> item.description.map(JsString).getOrElse(JsNull),
      "is_obsolete" -> JsBoolean(item.isObsolete)
    )
    def read(json: JsValue): BricklinkItem = {
      val obj = json.asJsObject
      val fields = obj.fields
      BricklinkItem(
        no = fields("no").convertTo[String],
        name = fields("name").convertTo[String],
        itemType = fields("type").convertTo[String],
        categoryId = fields("category_id").convertTo[Int],
        imageUrl = fields.get("image_url").flatMap(_.convertTo[Option[String]]),
        thumbnailUrl = fields.get("thumbnail_url").flatMap(_.convertTo[Option[String]]),
        weight = fields.get("weight").flatMap(v => if (v == JsNull) None else Some(v.convertTo[String])),
        dimX = fields.get("dim_x").flatMap(v => if (v == JsNull) None else Some(v.convertTo[String])),
        dimY = fields.get("dim_y").flatMap(v => if (v == JsNull) None else Some(v.convertTo[String])),
        dimZ = fields.get("dim_z").flatMap(v => if (v == JsNull) None else Some(v.convertTo[String])),
        yearReleased = fields.get("year_released").flatMap(v => if (v == JsNull) None else Some(v.convertTo[Int])),
        description = fields.get("description").flatMap(v => if (v == JsNull) None else Some(v.convertTo[String])),
        isObsolete = fields.get("is_obsolete").map(_.convertTo[Boolean]).getOrElse(false)
      )
    }
  }

  sealed trait Command
  final case class GetItem(itemType: String, itemNumber: String, replyTo: ActorRef[Response]) extends Command
  final case class GetItemNumberByElementId(elementId: String, replyTo: ActorRef[Response]) extends Command
  private final case class WrappedCacheResponse(cacheKey: String, response: DiskCache.Response, originalReplyTo: ActorRef[Response]) extends Command
  private final case class WrappedApiResponse(replyTo: ActorRef[Response], cacheKey: String, result: Response) extends Command

  sealed trait Response
  final case class ItemResponse(item: BricklinkItem) extends Response
  final case class ItemMappingResponse(itemNo: String, itemType: String) extends Response
  final case class Failed(reason: String) extends Response

  case class ApiMeta(code: String, message: String, description: String)
  case class ApiResponse(meta: ApiMeta, data: BricklinkItem)
  case class ItemMappingEntry(item: ItemMapping, colorId: Int, elementId: String)
  case class ItemMapping(no: String, `type`: String)
  case class ItemMappingApiResponse(meta: ApiMeta, data: List[ItemMappingEntry])

  implicit val itemMappingFormat: RootJsonFormat[ItemMapping] = jsonFormat2(ItemMapping)
  implicit val itemMappingEntryFormat: RootJsonFormat[ItemMappingEntry] = new RootJsonFormat[ItemMappingEntry] {
    def write(entry: ItemMappingEntry): JsValue = JsObject(
      "item" -> entry.item.toJson,
      "color_id" -> JsNumber(entry.colorId),
      "element_id" -> JsString(entry.elementId)
    )
    def read(json: JsValue): ItemMappingEntry = {
      val obj = json.asJsObject
      val fields = obj.fields
      ItemMappingEntry(
        item = fields("item").convertTo[ItemMapping],
        colorId = fields("color_id").convertTo[Int],
        elementId = fields("element_id").convertTo[String]
      )
    }
  }
  implicit val itemMappingApiResponseFormat: RootJsonFormat[ItemMappingApiResponse] = new RootJsonFormat[ItemMappingApiResponse] {
    def write(resp: ItemMappingApiResponse): JsValue = JsObject(
      "meta" -> resp.meta.toJson,
      "data" -> JsArray(resp.data.map(_.toJson))
    )
    def read(json: JsValue): ItemMappingApiResponse = {
      val obj = json.asJsObject
      ItemMappingApiResponse(
        meta = obj.fields("meta").convertTo[ApiMeta],
        data = obj.fields("data").convertTo[List[ItemMappingEntry]]
      )
    }
  }

  implicit val apiMetaFormat: RootJsonFormat[ApiMeta] = new RootJsonFormat[ApiMeta] {
    def write(m: ApiMeta): JsValue = JsObject(
      "code" -> JsString(m.code),
      "message" -> JsString(m.message),
      "description" -> JsString(m.description)
    )
    def read(json: JsValue): ApiMeta = json.asJsObject.getFields("code", "message", "description") match {
      case Seq(JsString(code), JsString(message), JsString(description)) =>
        ApiMeta(code, message, description)
      case Seq(JsNumber(code), JsString(message), JsString(description)) =>
        ApiMeta(code.toString, message, description)
      case Seq(code, JsString(message), JsString(description)) =>
        ApiMeta(code.toString, message, description)
    }
  }
  implicit val apiResponseFormat: RootJsonFormat[ApiResponse] = jsonFormat2(ApiResponse)

  private val BricklinkApiBase = "https://api.bricklink.com/api/store/v1"
  private val CacheTtlMs = 6 * 60 * 60 * 1000L // 6 hours

  def apply(cache: ActorRef[DiskCache.Command]): Behavior[Command] = Behaviors.setup { context =>
    val credentials = loadCredentials()
    credentials match {
      case Some(creds) =>
        context.log.info("BricklinkActor initialized with credentials")
        running(creds, cache)
      case None =>
        context.log.error("Failed to load Bricklink credentials - bricklink_ids.json not found or invalid")
        Behaviors.stopped
    }
  }

  private def running(credentials: BricklinkCredentials, cache: ActorRef[DiskCache.Command]): Behavior[Command] = {
    Behaviors.receive { (context, message) =>
      implicit val ec: ExecutionContext = context.executionContext

      message match {
        case GetItem(itemType, itemNumber, replyTo) =>
          val cacheKey = s"bricklink:item:$itemType:$itemNumber"
          context.messageAdapter[DiskCache.Response] { cacheResponse =>
            WrappedCacheResponse(cacheKey, cacheResponse, replyTo)
          } match {
            case adapter: ActorRef[DiskCache.Response] =>
              cache ! DiskCache.Fetch(cacheKey, adapter)
            case _ =>
              replyTo ! Failed("Failed to create cache adapter")
          }
          Behaviors.same

        case GetItemNumberByElementId(elementId, replyTo) =>
          val cacheKey = s"bricklink:mapping:$elementId"
          context.messageAdapter[DiskCache.Response] { cacheResponse =>
            WrappedCacheResponse(cacheKey, cacheResponse, replyTo)
          } match {
            case adapter: ActorRef[DiskCache.Response] =>
              cache ! DiskCache.Fetch(cacheKey, adapter)
            case _ =>
              replyTo ! Failed("Failed to create cache adapter")
          }
          Behaviors.same

        case WrappedCacheResponse(cacheKey, cacheResponse, replyTo) =>
          cacheResponse match {
            case DiskCache.FetchResult(_, value, insertedAt) =>
              val now = System.currentTimeMillis()
              if (now - insertedAt < CacheTtlMs) {
                if (cacheKey.startsWith("bricklink:mapping:")) {
                  Try(value.parseJson.convertTo[ItemMappingApiResponse]) match {
                    case Success(apiResponse) =>
                      context.log.debug(s"Cache hit for $cacheKey")
                      val mapping = apiResponse.data.head
                      replyTo ! ItemMappingResponse(mapping.item.no, mapping.item.`type`)
                    case Failure(ex) =>
                      context.log.debug(s"Cache parse error for $cacheKey, fetching from API")
                      fetchItemMappingFromApi(credentials, cacheKey, replyTo, context, cache)
                  }
                } else {
                  Try(value.parseJson.convertTo[BricklinkItem]) match {
                    case Success(item) =>
                      context.log.debug(s"Cache hit for $cacheKey")
                      replyTo ! ItemResponse(item)
                    case Failure(ex) =>
                      context.log.debug(s"Cache parse error for $cacheKey, fetching from API")
                      fetchFromApi(credentials, cacheKey, replyTo, context, cache)
                  }
                }
              } else {
                if (cacheKey.startsWith("bricklink:mapping:")) {
                  context.log.debug(s"Cache expired for $cacheKey, fetching from API")
                  fetchItemMappingFromApi(credentials, cacheKey, replyTo, context, cache)
                } else {
                  context.log.debug(s"Cache expired for $cacheKey, fetching from API")
                  fetchFromApi(credentials, cacheKey, replyTo, context, cache)
                }
              }
            case DiskCache.NotFound(_) =>
              context.log.debug(s"Cache miss for $cacheKey, fetching from API")
              if (cacheKey.startsWith("bricklink:mapping:")) {
                fetchItemMappingFromApi(credentials, cacheKey, replyTo, context, cache)
              } else {
                fetchFromApi(credentials, cacheKey, replyTo, context, cache)
              }
          }
          Behaviors.same

        case WrappedApiResponse(replyTo, cacheKey, result) =>
          replyTo ! result
          Behaviors.same
      }
    }
  }

  private def fetchFromApi(
    credentials: BricklinkCredentials,
    cacheKey: String,
    replyTo: ActorRef[Response],
    context: ActorContext[Command],
    cache: ActorRef[DiskCache.Command]
  ): Unit = {
    val parts = cacheKey.split(":")
    if (parts.length != 4) {
      replyTo ! Failed(s"Invalid cache key: $cacheKey")
      return
    }
    val itemType = parts(2)
    val itemNumber = parts(3)

    implicit val ec: ExecutionContext = context.executionContext
    implicit val system = context.system.classicSystem

    val url = s"$BricklinkApiBase/items/$itemType/$itemNumber"
    val oauthHeader = generateOAuthHeader(url, "GET", credentials)

    val authHeader = RawHeader("Authorization", oauthHeader)
    val acceptHeader = Accept(MediaTypes.`application/json`)

    val request = HttpRequest(
      uri = url,
      headers = Seq(authHeader, acceptHeader)
    )

    val responseFuture = Http().singleRequest(request).flatMap { response =>
      Unmarshal(response.entity).to[String].map { body =>
        (response.status.intValue, body)
      }
    }

    context.pipeToSelf(responseFuture) {
      case scala.util.Success((statusCode, body)) =>
        val result: Response = if (statusCode >= 200 && statusCode < 300) {
          Try(body.parseJson.convertTo[ApiResponse]) match {
            case Success(apiResponse) if apiResponse.meta.code == "200" =>
              val item = apiResponse.data
              val itemJson = item.toJson.prettyPrint
              cache ! DiskCache.Insert(cacheKey, itemJson)
              ItemResponse(item)
            case Success(apiResponse) =>
              Failed(s"BrickLink API error: ${apiResponse.meta.code} - ${apiResponse.meta.message}")
            case Failure(_) =>
              if (body.startsWith("{")) {
                Failed(s"Failed to parse BrickLink response: $body")
              } else {
                Failed(s"BrickLink API authentication failed (HTTP $statusCode). Body: $body")
              }
          }
        } else {
          if (body.startsWith("{")) {
            Try(body.parseJson.convertTo[ApiResponse]) match {
              case Success(apiResponse) =>
                Failed(s"BrickLink API error (HTTP $statusCode): ${apiResponse.meta.code} - ${apiResponse.meta.message}")
              case Failure(_) =>
                Failed(s"BrickLink API error (HTTP $statusCode): $body")
            }
          } else {
            Failed(s"BrickLink API error (HTTP $statusCode): $body")
          }
        }
        WrappedApiResponse(replyTo, cacheKey, result)
      case scala.util.Failure(ex) =>
        WrappedApiResponse(replyTo, cacheKey, Failed(s"Failed to fetch from BrickLink: ${ex.getMessage}"))
    }
  }

  private def fetchItemMappingFromApi(
    credentials: BricklinkCredentials,
    cacheKey: String,
    replyTo: ActorRef[Response],
    context: ActorContext[Command],
    cache: ActorRef[DiskCache.Command]
  ): Unit = {
    val elementId = cacheKey.replace("bricklink:mapping:", "")
    implicit val ec: ExecutionContext = context.executionContext
    implicit val system = context.system.classicSystem

    val url = s"$BricklinkApiBase/item_mapping/$elementId"
    val oauthHeader = generateOAuthHeader(url, "GET", credentials)

    val authHeader = RawHeader("Authorization", oauthHeader)
    val acceptHeader = Accept(MediaTypes.`application/json`)

    val request = HttpRequest(
      uri = url,
      headers = Seq(authHeader, acceptHeader)
    )

    val responseFuture = Http().singleRequest(request).flatMap { response =>
      Unmarshal(response.entity).to[String].map { body =>
        (response.status.intValue, body)
      }
    }

    context.pipeToSelf(responseFuture) {
      case scala.util.Success((statusCode, body)) =>
        val result: Response = if (statusCode >= 200 && statusCode < 300) {
          Try(body.parseJson.convertTo[ItemMappingApiResponse]) match {
            case Success(apiResponse) if apiResponse.meta.code == "200" =>
              val mapping = apiResponse.data.head
              val mappingJson = apiResponse.toJson.prettyPrint
              cache ! DiskCache.Insert(cacheKey, mappingJson)
              ItemMappingResponse(mapping.item.no, mapping.item.`type`)
            case Success(apiResponse) =>
              Failed(s"BrickLink API error: ${apiResponse.meta.code} - ${apiResponse.meta.message}")
            case Failure(ex) =>
              Failed(s"Failed to parse BrickLink response: $body")
          }
        } else {
          if (body.startsWith("{")) {
            Try(body.parseJson.convertTo[ApiResponse]) match {
              case Success(apiResponse) =>
                Failed(s"BrickLink API error (HTTP $statusCode): ${apiResponse.meta.code} - ${apiResponse.meta.message}")
              case Failure(_) =>
                Failed(s"BrickLink API error (HTTP $statusCode): $body")
            }
          } else {
            Failed(s"BrickLink API error (HTTP $statusCode): $body")
          }
        }
        WrappedApiResponse(replyTo, cacheKey, result)
      case scala.util.Failure(ex) =>
        WrappedApiResponse(replyTo, cacheKey, Failed(s"Failed to fetch from BrickLink: ${ex.getMessage}"))
    }
  }

  private def generateOAuthHeader(url: String, method: String, credentials: BricklinkCredentials): String = {
    val timestamp = (System.currentTimeMillis() / 1000).toString
    val nonce = java.util.UUID.randomUUID().toString.replace("-", "")

    val signatureBaseString = buildSignatureBaseString(url, method, credentials.consumerKey, credentials.tokenValue, timestamp, nonce)
    val signature = computeHmacSha1(signatureBaseString, s"${credentials.consumerSecret}&${credentials.tokenSecret}")

    val encodedSignature = percentEncode(signature)

    s"""OAuth realm="", oauth_consumer_key="${credentials.consumerKey}", oauth_token="${credentials.tokenValue}", oauth_signature_method="HMAC-SHA1", oauth_signature="$encodedSignature", oauth_timestamp="$timestamp", oauth_nonce="$nonce", oauth_version="1.0""""
  }

  private def buildSignatureBaseString(url: String, method: String, consumerKey: String, tokenValue: String, timestamp: String, nonce: String): String = {
    val normalizedUrl = normalizeUrl(url)

    val params = Seq(
      s"oauth_consumer_key=$consumerKey",
      s"oauth_nonce=$nonce",
      s"oauth_signature_method=HMAC-SHA1",
      s"oauth_timestamp=$timestamp",
      s"oauth_token=$tokenValue",
      s"oauth_version=1.0"
    ).sorted.mkString("&")

    val encodedParams = percentEncode(params)

    s"${method.toUpperCase}&${percentEncode(normalizedUrl)}&$encodedParams"
  }

  private def percentEncode(s: String): String = {
    encode(s, "UTF-8")
      .replace("+", "%20")
      .replace("*", "%2A")
      .replace("%7E", "~")
  }

  private def normalizeUrl(url: String): String = {
    val uri = new URI(url)
    val scheme = uri.getScheme.toLowerCase
    val host = uri.getHost.toLowerCase
    val port = uri.getPort
    val path = uri.getPath

    val schemeAndAuthority = s"$scheme://$host${if (port > 0) s":$port" else ""}"
    s"$schemeAndAuthority$path"
  }

  private def computeHmacSha1(data: String, key: String): String = {
    val mac = Mac.getInstance("HmacSHA1")
    mac.init(new SecretKeySpec(key.getBytes("UTF-8"), "HmacSHA1"))
    Base64.getEncoder.encodeToString(mac.doFinal(data.getBytes("UTF-8"))).replace("\r\n", "")
  }

  private def loadCredentials(): Option[BricklinkCredentials] = {
    val file = new java.io.File("bricklink_ids.json")
    if (!file.exists()) {
      return None
    }
    Try {
      val content = scala.io.Source.fromFile(file).mkString
      val json = content.parseJson.asJsObject
      BricklinkCredentials(
        consumerKey = json.fields("consumer_key").asInstanceOf[JsString].value,
        consumerSecret = json.fields("consumer_secret").asInstanceOf[JsString].value,
        tokenValue = json.fields("token_value").asInstanceOf[JsString].value,
        tokenSecret = json.fields("token_secret").asInstanceOf[JsString].value
      )
    } match {
      case Success(creds) => Some(creds)
      case Failure(ex) =>
        println(s"Failed to load Bricklink credentials: ${ex.getMessage}")
        None
    }
  }
}
