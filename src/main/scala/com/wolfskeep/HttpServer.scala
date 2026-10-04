package com.wolfskeep

import akka.actor.typed.ActorRef
import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.AskPattern._
import akka.http.scaladsl.Http
import akka.http.scaladsl.ConnectionContext
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.StandardRoute
import akka.http.scaladsl.server.Route
import akka.stream.scaladsl.Source
import akka.stream.Materializer
import akka.util.Timeout
import akka.util.ByteString
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import akka.stream.scaladsl.StreamConverters
import java.io.{BufferedInputStream, InputStreamReader}
import java.net.URLEncoder
import com.wolfskeep.rebrickable.{Color, Data, Element, InventoryPart, Part, RebrickableHolder}

object HttpServer {
  val HttpPort = 37080
  val HttpsPort = 37443
  val DefaultMaxImageWidth = 100
  val StartTime = System.currentTimeMillis()
  val StillProcessingMessage =
    "The service is still processing your request - it may be busy. Please try again in a little while."
  val ProcessingFailedMessage =
    "The service could not process your request. Please try again."

  def cacheBustJsPath(filename: String): String = s"/$filename?t=$StartTime"

  def start(
    partsProcessor: ActorRef[PartsProcessor.Command],
    rebrickableDataActor: ActorRef[RebrickableHolder.Command],
    imageResolver: ActorRef[ImageResolver.Command],
    actorSystem: ActorSystem[_]
  )(implicit ec: ExecutionContext, materializer: Materializer): Future[Http.ServerBinding] = {
    val sslContext = SslContextBuilder.buildSslContext()
    val httpsConnectionContext = ConnectionContext.https(sslContext)

    val route = Routes.all(actorSystem, partsProcessor, rebrickableDataActor, imageResolver)

    val classicSystem = actorSystem.classicSystem
    implicit val system = classicSystem

    val httpBinding = Http().newServerAt("0.0.0.0", HttpPort).bind(route)
    Http().newServerAt("0.0.0.0", HttpsPort).enableHttps(httpsConnectionContext).bind(route)

    actorSystem.log.info(s"HTTP server started on port $HttpPort")
    actorSystem.log.info(s"HTTPS server started on port $HttpsPort")

    httpBinding
  }
}

object Routes {
  def all(
    actorSystem: ActorSystem[_],
    partsProcessor: ActorRef[PartsProcessor.Command],
    rebrickableDataActor: ActorRef[RebrickableHolder.Command],
    imageResolver: ActorRef[ImageResolver.Command],
    timeouts: Timeouts.Web = Timeouts.web
  )(implicit ec: ExecutionContext, materializer: Materializer): Route = {
    implicit val scheduler: akka.actor.typed.Scheduler = actorSystem.scheduler
    val classicScheduler: akka.actor.Scheduler = actorSystem.classicSystem.scheduler
    val imageAskTimeout: Timeout = Timeout(3.seconds)

    def askLdrawImage(colorId: Int, partNumber: String): Future[ImageResolver.LdrawImageResponse] = {
      implicit val askTimeout: Timeout = imageAskTimeout
      imageResolver.ask(ref => ImageResolver.GetLdrawImage(colorId, partNumber, ref))
    }

    def askBricksetImageUrl(partNumber: String, elementId: Option[String]): Future[ImageResolver.BricksetImageResponse] = {
      implicit val askTimeout: Timeout = imageAskTimeout
      imageResolver.ask(ref => ImageResolver.GetBricksetImageUrl(partNumber, elementId, ref))
    }

    def waitForImage[T](askOnce: () => Future[T], isPending: T => Boolean): Future[Option[T]] = {
      val deadline = timeouts.imageWait.fromNow
      def loop(): Future[Option[T]] =
        askOnce().flatMap { answer =>
          if (!isPending(answer)) Future.successful(Some(answer))
          else if (deadline.isOverdue()) Future.successful(None)
          else akka.pattern.after(timeouts.imageRetryAfter, classicScheduler)(loop())
        }.recover { case _: Throwable => None }
      loop()
    }

    def errorPage(status: StatusCode, message: String): StandardRoute =
      complete((status, HttpEntity(ContentTypes.`text/html(UTF-8)`,
        partsSorterHtml(Nil, Some(message), Map.empty))))

    def failureResponse(ex: Throwable): StandardRoute = ex match {
      case _: java.util.concurrent.TimeoutException | _: akka.pattern.AskTimeoutException =>
        errorPage(StatusCodes.ServiceUnavailable, HttpServer.StillProcessingMessage)
      case _ =>
        errorPage(StatusCodes.InternalServerError, HttpServer.ProcessingFailedMessage)
    }

    val retryAfterImageResponse: Route =
      respondWithHeader(RawHeader("Retry-After", timeouts.imageRetryAfter.toSeconds.toString)) {
        complete(StatusCodes.ServiceUnavailable)
      }

    val postRoute: Route = post {
      path("parts-sorter") {
        withRequestTimeout(timeouts.httpServerRequestTimeout) {
          formField("setNumber".as[String].?) { setNumberOpt =>
            setNumberOpt match {
              case Some(setNumber) if setNumber.trim.nonEmpty =>
                val result = getSetInventory(rebrickableDataActor, setNumber, timeouts.dataAsk)
                  .recover { case ex: NoSuchElementException =>
                    (List.empty[ColoredPart], ex.getMessage, Map.empty[String, Int])
                  }
                  .flatMap { case (coloredParts, setInfo, colorNameToId) =>
                    if (coloredParts.isEmpty) {
                      Future.successful((List.empty[MatchedPart], setInfo, colorNameToId))
                    } else {
                      processParts(coloredParts, partsProcessor, setInfo, colorNameToId, timeouts.processAsk)
                    }
                  }

                onComplete(result) {
                  case scala.util.Success((results, setInfo, colorNameToId)) =>
                    complete(HttpEntity(ContentTypes.`text/html(UTF-8)`,
                      partsSorterHtml(results, Some(setInfo), colorNameToId)))
                  case scala.util.Failure(ex) =>
                    failureResponse(ex)
                }

              case _ =>
                fileUpload("inputFile") { case (fileInfo, byteSource) =>
                  val coloredPartsF = {
                    implicit val dataTimeout: Timeout = Timeout(timeouts.dataAsk)
                    for {
                      data <- rebrickableDataActor.ask(RebrickableHolder.GetData(_))
                      colorIdToName = data.colors.map(c => c.id -> c.name).toMap
                      elementIdToPartColor = data.elements.map { e =>
                        val colorName = data.colorIdToColor.get(e.colorId).map(_.name).getOrElse(s"unknown-${e.colorId}")
                        val partName = data.partNumToPart.get(e.partNum).map(_.name).getOrElse("")
                        e.elementId -> (e.partNum, colorName, partName)
                      }.toMap
                      coloredParts <- processUploadedFile(byteSource, colorIdToName, elementIdToPartColor)
                    } yield (coloredParts, data.colors.map(c => c.name -> c.id).toMap)
                  }

                  val processedParts = coloredPartsF.flatMap { case (coloredParts, colorNameToId) =>
                    implicit val processTimeout: Timeout = Timeout(timeouts.processAsk)
                    partsProcessor.ask(PartsProcessor.ProcessParts(coloredParts, _))
                      .map { case PartsProcessor.ProcessedParts(results) => (results, colorNameToId) }
                  }

                  onComplete(processedParts) {
                    case scala.util.Success((results, colorNameToId)) =>
                      complete(HttpEntity(ContentTypes.`text/html(UTF-8)`,
                        partsSorterHtml(results, Some(s"Uploaded file: ${fileInfo.fileName}"), colorNameToId)))
                    case scala.util.Failure(ex) =>
                      failureResponse(ex)
                  }
                }
            }
          }
        }
      }
    }

    concat(
      pathSingleSlash(redirect("/parts-sorter", StatusCodes.Found)),
      get {
        path("parts-sorter") {
          complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, partsSorterHtml(Nil, None, Map.empty)))
        }
      },
      postRoute,
      get {
        path("part_images" / "brickset" / Segment) { partNumber =>
          parameter("element".as[String].?) { elementId =>
            val answer = waitForImage[ImageResolver.BricksetImageResponse](
              () => askBricksetImageUrl(partNumber, elementId),
              _ == ImageResolver.BricksetImagePending
            )
            onSuccess(answer) {
              case Some(ImageResolver.BricksetImageResolved(url)) =>
                redirect(url, StatusCodes.Found)
              case Some(ImageResolver.BricksetImageUnavailable) =>
                complete(StatusCodes.NotFound)
              case _ =>
                retryAfterImageResponse
            }
          }
        }
      },
      get {
        path("part_images" / Segment / Segment) { case (colorIdStr, partNumberWithExt) =>
          val partNumber = partNumberWithExt.stripSuffix(".png")

          colorIdStr.toIntOption match {
            case Some(colorId) =>
              val answer = waitForImage[ImageResolver.LdrawImageResponse](
                () => askLdrawImage(colorId, partNumber),
                _ == ImageResolver.LdrawImagePending
              )
              onSuccess(answer) {
                case Some(ImageResolver.LdrawImageReady(bytes)) =>
                  complete(HttpEntity(ContentType(MediaTypes.`image/png`), bytes))
                case Some(ImageResolver.LdrawImageUnavailable) =>
                  complete(StatusCodes.NotFound)
                case _ =>
                  retryAfterImageResponse
              }
            case None =>
              complete(StatusCodes.BadRequest)
          }
        }
      },
      get {
        path("parts-sorter.css") {
          getFromResource("parts-sorter.css")
        }
      },
      get {
        path("parts-sorter.js") {
          getFromResource("parts-sorter.js")
        }
      },
      get {
        path("partsSorterImages.js") {
          getFromResource("partsSorterImages.js")
        }
      },
      get {
        path("columnOrder.js") {
          getFromResource("columnOrder.js")
        }
      }
    )
  }

  private def getSetInventory(
    rebrickableDataActor: ActorRef[RebrickableHolder.Command],
    setNumber: String,
    dataAsk: FiniteDuration
  )(implicit scheduler: akka.actor.typed.Scheduler, ec: ExecutionContext): Future[(List[ColoredPart], String, Map[String, Int])] = {
    implicit val dataTimeout: Timeout = Timeout(dataAsk)
    rebrickableDataActor.ask(RebrickableHolder.GetData(_)).map { data =>
      val trimmedSetNumber = setNumber.trim

      val setOpt = data.sets.find(_.setNum == trimmedSetNumber)
        .orElse {
          if (!trimmedSetNumber.endsWith("-1")) {
            data.sets.find(_.setNum == s"$trimmedSetNumber-1")
          } else None
        }

      val set = setOpt.getOrElse {
        throw new NoSuchElementException(s"No set found for $trimmedSetNumber")
      }

      val inventory = data.inventories
        .filter(_.setNum == set.setNum)
        .maxByOption(_.id)
        .getOrElse {
          throw new NoSuchElementException(s"No inventory found for ${set.setNum}")
        }

      val partMap = data.partNumToPart
      val colorMap = data.colorIdToColor
      val elementMap = data.partNumColorIdToElement

      val coloredParts = data.inventoryParts
        .filter(p => p.inventoryId == inventory.id && !p.isSpare)
        .map { invPart =>
          ColoredPart(
            partNumber = invPart.partNum,
            color = colorMap.get(invPart.colorId).map(_.name).getOrElse(""),
            quantity = invPart.quantity,
            name = partMap.get(invPart.partNum).map(_.name).getOrElse(""),
            elementId = elementMap.get((invPart.partNum, invPart.colorId)).map(_.elementId.toString)
          )
        }

      (coloredParts, s"${set.setNum}: ${set.name}", data.colors.map(c => c.name -> c.id).toMap)
    }
  }

  private def processParts(
    coloredParts: List[ColoredPart],
    partsProcessor: ActorRef[PartsProcessor.Command],
    setName: String,
    colorNameToId: Map[String, Int],
    processAsk: FiniteDuration
  )(implicit ec: ExecutionContext, scheduler: akka.actor.typed.Scheduler): Future[(List[MatchedPart], String, Map[String, Int])] = {
    implicit val processTimeout: Timeout = Timeout(processAsk)
    partsProcessor.ask(PartsProcessor.ProcessParts(coloredParts, _)).map {
      case PartsProcessor.ProcessedParts(matchedParts) =>
        (matchedParts, setName, colorNameToId)
    }
  }

  private def processUploadedFile(
    byteSource: Source[ByteString, _],
    colorIdToName: Map[Int, String],
    elementIdToPartColor: Map[Long, (String, String, String)]
  )(implicit ec: ExecutionContext, materializer: Materializer): Future[List[ColoredPart]] = Future {
    val inputStream = byteSource.runWith(StreamConverters.asInputStream())
    val bufferedStream = new BufferedInputStream(inputStream, 8192)
    bufferedStream.mark(8192)

    try {
      new StudioIoReader().readColoredParts(bufferedStream)
    } catch {
      case e: Exception =>
        bufferedStream.reset()
        val csvReader = new CsvReader()
        csvReader.readColoredPartsFromReader(new InputStreamReader(bufferedStream), colorIdToName, elementIdToPartColor)
    } finally {
      inputStream.close()
    }
  }

  def resolveColorId(color: String, colorNameToId: Map[String, Int]): Option[Int] =
    colorNameToId.get(color).orElse {
      StudioIoReader.colorMap.values.find(_.equalsIgnoreCase(color)).flatMap(colorNameToId.get)
    }

  private def urlEncode(s: String): String = URLEncoder.encode(s, "UTF-8")

  def partsSorterHtml(
    results: List[MatchedPart],
    sourceMessage: Option[String],
    colorNameToId: Map[String, Int] = Map.empty
  ): String = {
    s"""<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>LEGO Parts Sorter</title>
    <link rel="stylesheet" href=${HttpServer.cacheBustJsPath("parts-sorter.css")}>
</head>
<body>
    <div class="description">
        <h1>LEGO Parts Sorter</h1>
        <p>This is a web page for sorting LEGO parts lists according to Tom Alphin's <a href="http://brickarchitect.com/parts/">LEGO Parts Guide</a>.</p>
    </div>

    <div class="input-section">
        <h2>Input</h2>
        <form method="POST" action="/parts-sorter" enctype="multipart/form-data" id="uploadForm">
            <div style="margin-bottom: 15px;">
                <label for="setNumber">LEGO Set Number:</label><br>
                <input type="text" name="setNumber" id="setNumber" placeholder="e.g., 21321-1">
            </div>
            <div>
                <label for="inputFile">Or upload file (.csv or .io):</label><br>
                <input type="file" name="inputFile" id="inputFile" accept=".csv,.io"><br>
                Supported formats include Studio model file, Studio model summary export, BrickSet inventory, Rebrickable inventory, or LEGO Pick-A-Brick csv.
            </div>
        </form>
    </div>

    <div class="output-section">
        <h2>Output</h2>
        ${if (results.isEmpty && isErrorMessage(sourceMessage)) {
            s"""<p class="error-message"><h2>${escapeHtml(sourceMessage.get)}</h2></p>"""
        } else if (results.isEmpty) {
            """<p class="no-results">Enter a LEGO Set Number or upload a CSV file to see sorted results.</p>"""
        } else {
            val sourceHtml = sourceMessage.map(msg => s"""<div class="source-info"><span class="file-name">${escapeHtml(msg)}</span><button id="resetColumnsBtn" onclick="resetColumnOrder()">Reset Columns</button></div>""").getOrElse("")
            s"""${sourceHtml}<table>
                <thead>
                    <tr>
                        <th draggable="true" data-col-type="category" data-col-id="category">category</th>
                        <th draggable="true" data-col-type="category" data-col-id="category2">category2</th>
                        <th draggable="true" data-col-type="category" data-col-id="category3">category3</th>
                        <th draggable="true" data-col-type="category" data-col-id="category4">category4</th>
                        <th draggable="true" data-col-type="normal" data-col-id="image">image</th>
                        <th draggable="true" data-col-type="normal" data-col-id="color">color</th>
                        <th draggable="true" data-col-type="normal" data-col-id="quantity">quantity</th>
                        <th draggable="true" data-col-type="normal" data-col-id="name">name</th>
                        <th draggable="true" data-col-type="normal" data-col-id="partNumber">partNumber</th>
                    </tr>
                </thead>
                <tbody>
                    ${
                        val maxTaxonomyWidth = results.flatMap(_.legoPart.flatMap(_.imageWidth))
                          .map(_.toDouble)
                          .maxOption
                          .getOrElse(HttpServer.DefaultMaxImageWidth.toDouble)
                          .toInt

                        results.map { mp =>
                        val catNames = mp.legoPart.map(_.categories.map(_.name)).getOrElse(Nil)
                        val guessedMarker = if (mp.categoriesGuessed && catNames.nonEmpty) " (guessed)" else ""
                        val legoPart = mp.legoPart
                        val imageWidth = legoPart.flatMap(_.imageWidth)
                        val imageHeight = legoPart.flatMap(_.imageHeight)
                        val imageHtml = mp.legoPart match {
                          case Some(part) =>
                            part.imageUrl match {
                              case Some(url) =>
                                (imageWidth, imageHeight) match {
                                  case (Some(w), Some(h)) => s"""<img src="${escapeHtml(url)}" width="${escapeHtml(w)}" height="${escapeHtml(h)}" />"""
                                  case (Some(w), None) => s"""<img src="${escapeHtml(url)}" width="${escapeHtml(w)}" />"""
                                  case (None, Some(h)) => s"""<img src="${escapeHtml(url)}" height="${escapeHtml(h)}" />"""
                                  case _ => s"""<img src="${escapeHtml(url)}" style="max-width: ${maxTaxonomyWidth}px" />"""
                                }
                              case None =>
                                resolveColorId(mp.coloredPart.color, colorNameToId).map { colorId =>
                                  val elementQuery = mp.coloredPart.elementId.fold("")(id => s"?element=${urlEncode(id)}")
                                  s"""<img alt="" style="max-width: ${maxTaxonomyWidth}px" data-image-ldraw="/part_images/$colorId/${escapeHtml(mp.coloredPart.partNumber)}.png" data-image-brickset="/part_images/brickset/${escapeHtml(mp.coloredPart.partNumber)}$elementQuery" />"""
                                }.getOrElse("")
                            }
                          case None =>
                            ""
                        }
                        s"""<tr>
                            <td data-col-id="category">${escapeHtml(catNames.headOption.getOrElse(""))}$guessedMarker</td>
                            <td data-col-id="category2">${escapeHtml(catNames.lift(1).getOrElse(""))}</td>
                            <td data-col-id="category3">${escapeHtml(catNames.lift(2).getOrElse(""))}</td>
                            <td data-col-id="category4">${escapeHtml(catNames.lift(3).getOrElse(""))}</td>
                            <td data-col-id="image">${imageHtml}</td>
                            <td data-col-id="color">${escapeHtml(mp.coloredPart.color)}</td>
                            <td data-col-id="quantity">${escapeHtml(mp.coloredPart.quantity.toString)}</td>
                            <td data-col-id="name">${escapeHtml(mp.coloredPart.name)}</td>
                            <td data-col-id="partNumber">${escapeHtml(mp.coloredPart.partNumber)}</td>
                        </tr>"""
                    }.mkString}
                </tbody>
            </table>"""
        }}
    </div>

    <script src=${HttpServer.cacheBustJsPath("columnOrder.js")}></script>
    <script src=${HttpServer.cacheBustJsPath("partsSorterImages.js")}></script>
    <script src=${HttpServer.cacheBustJsPath("parts-sorter.js")}></script>
</body>
</html>"""
  }

  private def escapeHtml(s: String): String = {
    s.replace("&", "&amp;")
      .replace("<", "&lt;")
      .replace(">", "&gt;")
      .replace("\"", "&quot;")
      .replace("'", "&#39;")
  }

  private def isErrorMessage(sourceMessage: Option[String]): Boolean = {
    sourceMessage.exists { msg =>
      msg.startsWith("No set found for") ||
      msg.startsWith("No inventory found for") ||
      msg.startsWith("The service ")
    }
  }
}
