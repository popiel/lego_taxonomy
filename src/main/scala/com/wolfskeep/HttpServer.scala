package com.wolfskeep

import akka.actor.typed.ActorRef
import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.AskPattern._
import akka.http.scaladsl.Http
import akka.http.scaladsl.ConnectionContext
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.scaladsl.FileIO
import akka.stream.scaladsl.Source
import akka.stream.Materializer
import akka.util.Timeout
import akka.util.ByteString
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import akka.stream.scaladsl.StreamConverters
import java.io.{BufferedInputStream, InputStreamReader}
import java.util.zip.ZipFile
import com.wolfskeep.rebrickable.{Color, Data, Element, InventoryPart, Part, RebrickableHolder, LDrawImageFetcher}

object HttpServer {
  val HttpPort = 37080
  val HttpsPort = 37443
  val DefaultMaxImageWidth = 100

  def start(
    partsProcessor: ActorRef[PartsProcessor.Command],
    rebrickableDataActor: ActorRef[RebrickableHolder.Command],
    actorSystem: ActorSystem[_]
  )(implicit ec: ExecutionContext, materializer: Materializer): Future[Http.ServerBinding] = {
    val sslContext = SslContextBuilder.buildSslContext()
    val httpsConnectionContext = ConnectionContext.https(sslContext)

    val route = Routes.all(actorSystem, partsProcessor, rebrickableDataActor)

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
    rebrickableDataActor: ActorRef[RebrickableHolder.Command]
  )(implicit ec: ExecutionContext, materializer: Materializer): Route = {
    implicit val scheduler: akka.actor.typed.Scheduler = actorSystem.scheduler
    val imageFetcher = new LDrawImageFetcher()(actorSystem)

    concat(
      pathSingleSlash(redirect("/parts-sorter", StatusCodes.Found)),
      get {
        path("parts-sorter") {
          complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, partsSorterHtml(Nil, None)))
        }
      },
      post {
        path("parts-sorter") {
          formField("setNumber".as[String].?) { setNumberOpt =>
            setNumberOpt match {
              case Some(setNumber) if setNumber.trim.nonEmpty =>
                val result = getSetInventory(rebrickableDataActor, setNumber)
                  .recover { case ex: NoSuchElementException => (Nil, ex.getMessage) }
                  .flatMap { case (coloredParts, setInfo) =>
                    if (coloredParts.isEmpty) {
                      Future.successful((Nil, setInfo))
                    } else {
                      processParts(coloredParts, partsProcessor, setInfo)
                    }
                  }

                onSuccess(result) { case (results, setInfo) =>
                  complete(HttpEntity(ContentTypes.`text/html(UTF-8)`,
                    partsSorterHtml(results, Some(setInfo))))
                }

              case _ =>
                fileUpload("inputFile") { case (fileInfo, byteSource) =>
                  implicit val timeout: Timeout = Timeout(10.seconds)
                  val coloredPartsF = processUploadedFile(byteSource)
                  val processedParts = coloredPartsF.flatMap { coloredParts =>
                    partsProcessor.ask(PartsProcessor.ProcessParts(coloredParts, _))
                  }
                  onSuccess(processedParts) {
                    case PartsProcessor.ProcessedParts(results) =>
                      complete(HttpEntity(ContentTypes.`text/html(UTF-8)`,
                        partsSorterHtml(results, Some(s"Uploaded file: ${fileInfo.fileName}"))))
                  }
                }
            }
          }
        }
      },
      get {
        path("part_images" / Segment / Segment) { case (colorIdStr, partNumberWithExt) =>
          val colorId = colorIdStr.toIntOption
          val partNumber = partNumberWithExt.stripSuffix(".png")
          
          colorId match {
            case Some(cid) =>
              val imageBytes = imageFetcher.getImageFromZip(cid, partNumber)
              
              imageBytes match {
                case Some(bytes) =>
                  val httpEntity = HttpEntity.apply(ContentType(MediaTypes.`image/png`), bytes)
                  complete(httpEntity)
                case None =>
                  complete(StatusCodes.NotFound)
              }
            case None =>
              complete(StatusCodes.BadRequest)
          }
        }
      }
    )
  }

  private def getSetInventory(
    rebrickableDataActor: ActorRef[RebrickableHolder.Command],
    setNumber: String
  )(implicit scheduler: akka.actor.typed.Scheduler, ec: ExecutionContext): Future[(List[ColoredPart], String)] = {
    implicit val timeout: Timeout = Timeout(30.seconds)

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

      (coloredParts, s"${set.setNum}: ${set.name}")
    }
  }

  private def processParts(
    coloredParts: List[ColoredPart],
    partsProcessor: ActorRef[PartsProcessor.Command],
    setName: String
  )(implicit ec: ExecutionContext, scheduler: akka.actor.typed.Scheduler): Future[(List[MatchedPart], String)] = {
    implicit val timeout: Timeout = Timeout(30.seconds)
    partsProcessor.ask(PartsProcessor.ProcessParts(coloredParts, _)).map {
      case PartsProcessor.ProcessedParts(matchedParts) =>
        (matchedParts, setName)
    }
  }

  private def processUploadedFile(byteSource: Source[ByteString, _])(implicit ec: ExecutionContext, materializer: Materializer): Future[List[ColoredPart]] = Future {
    val inputStream = byteSource.runWith(StreamConverters.asInputStream())
    val bufferedStream = new BufferedInputStream(inputStream, 8192)
    bufferedStream.mark(8192)

    try {
      new StudioIoReader().readColoredParts(bufferedStream)
    } catch {
      case e: Exception =>
        bufferedStream.reset()
        val csvReader = new CsvReader()
        csvReader.readColoredPartsFromReader(new InputStreamReader(bufferedStream))
    } finally {
      inputStream.close()
    }
  }

  def partsSorterHtml(results: List[MatchedPart], sourceMessage: Option[String]): String = {
    s"""<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>LEGO Parts Sorter</title>
    <style>
        body {
            font-family: Arial, sans-serif;
            margin: 20px;
            max-width: 1200px;
            margin: 0 auto;
            padding: 20px;
        }
        h1 {
            color: #333;
        }
        .description {
            background-color: #f5f5f5;
            padding: 15px;
            border-radius: 5px;
            margin-bottom: 20px;
        }
        .input-section {
            background-color: #fff;
            border: 1px solid #ddd;
            padding: 20px;
            border-radius: 5px;
            margin-bottom: 20px;
        }
        .input-section input[type="file"] {
            margin-top: 10px;
        }
        .output-section {
            margin-top: 20px;
        }
        table {
            width: 100%;
            border-collapse: collapse;
            margin-top: 10px;
        }
        th, td {
            border: 1px solid #ddd;
            padding: 8px;
            text-align: left;
            font-size: 14px;
        }
        th {
            background-color: #4CAF50;
            color: white;
        }
        th[draggable="true"] {
            cursor: grab;
            user-select: none;
        }
        th.dragging {
            opacity: 0.5;
            background-color: #ddd;
        }
        th.drag-over {
            background-color: #b3e5fc;
            outline: 2px solid #0288D1;
            outline-offset: -2px;
        }
        .drop-indicator {
            position: absolute;
            top: 0;
            bottom: 0;
            width: 6px;
            background-color: #00E676;
            pointer-events: none;
            z-index: 10;
            box-shadow: 0 0 8px #00E676;
        }
        .drop-indicator.before {
            left: -3px;
        }
        .drop-indicator.after {
            right: -3px;
        }
        .source-info {
            display: flex;
            align-items: center;
            gap: 15px;
            margin-bottom: 10px;
        }
        .file-name {
            font-weight: bold;
            color: #333;
        }
        #resetColumnsBtn {
            padding: 5px 10px;
            background-color: #f0f0f0;
            border: 1px solid #ccc;
            border-radius: 4px;
            cursor: pointer;
        }
        #resetColumnsBtn:hover {
            background-color: #e0e0e0;
        }
        tr:nth-child(even) {
            background-color: #f2f2f2;
        }
        .no-results {
            color: #666;
            font-style: italic;
        }
        .error-message {
            color: #c00;
            font-weight: bold;
        }
    </style>
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
                <input type="file" name="inputFile" id="inputFile" accept=".csv,.io">
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
                        val imageUrl = legoPart.flatMap(_.imageUrl)
                        val imageWidth = legoPart.flatMap(_.imageWidth)
                        val imageHeight = legoPart.flatMap(_.imageHeight)
                        val imageHtml = (imageUrl, imageWidth, imageHeight) match {
                          case (Some(url), Some(w), Some(h)) => s"""<img src="${escapeHtml(url)}" width="${escapeHtml(w)}" height="${escapeHtml(h)}" />"""
                          case (Some(url), Some(w), None) => s"""<img src="${escapeHtml(url)}" width="${escapeHtml(w)}" />"""
                          case (Some(url), None, Some(h)) => s"""<img src="${escapeHtml(url)}" height="${escapeHtml(h)}" />"""
                          case (Some(url), None, None) => s"""<img src="${escapeHtml(url)}" style="max-width: ${maxTaxonomyWidth}px" />"""
                          case _ => ""
                        }
                        s"""<tr>
                            <td data-col-id="category">${escapeHtml(catNames.headOption.getOrElse(""))}$guessedMarker</td>
                            <td data-col-id="category2">${escapeHtml(catNames.lift(1).getOrElse(""))}</td>
                            <td data-col-id="category3">${escapeHtml(catNames.lift(2).getOrElse(""))}</td>
                            <td data-col-id="category4">${escapeHtml(catNames.lift(3).getOrElse(""))}</td>
                            <td data-col-id="image">${imageHtml}</td>
                            <td data-col-id="color">${escapeHtml(mp.coloredPart.color)}</td>
                            <td data-col-id="quantity">${mp.coloredPart.quantity}</td>
                            <td data-col-id="name">${escapeHtml(mp.coloredPart.name)}</td>
                            <td data-col-id="partNumber">${escapeHtml(mp.coloredPart.partNumber)}</td>
                        </tr>"""
                    }.mkString}
                </tbody>
            </table>"""
        }}
    </div>

    <script>
        (function() {
            const CATEGORY_COLUMNS = ['category', 'category2', 'category3', 'category4'];
            const CATEGORY_START = 0;
            const CATEGORY_END = 3;

            let currentOrder = [...CATEGORY_COLUMNS, 'image', 'color', 'quantity', 'name', 'partNumber'];
            let draggedColId = null;
            let draggedColType = null;
            let draggedElement = null;

            document.addEventListener('DOMContentLoaded', init);

            function init() {
                initDesktopDragDrop();
                initTouchDragDrop();
                document.getElementById('resetColumnsBtn')?.addEventListener('click', resetColumnOrder);

                document.getElementById('inputFile').addEventListener('change', function() {
                    if (this.files.length > 0) {
                        document.getElementById('setNumber').value = '';
                        document.getElementById('uploadForm').submit();
                    }
                });
                document.getElementById('setNumber').addEventListener('keydown', function(e) {
                    if (e.key === 'Enter' && this.value.trim() !== '') {
                        document.getElementById('inputFile').value = '';
                        document.getElementById('uploadForm').submit();
                    }
                });
            }

            function initDesktopDragDrop() {
                document.querySelectorAll('th[draggable="true"]').forEach(th => {
                    th.addEventListener('dragstart', handleDragStart);
                    th.addEventListener('dragend', handleDragEnd);
                    th.addEventListener('dragover', handleDragOver);
                    th.addEventListener('dragleave', handleDragLeave);
                    th.addEventListener('drop', handleDrop);
                });
            }

            function initTouchDragDrop() {
                document.querySelectorAll('th[draggable="true"]').forEach(th => {
                    th.addEventListener('touchstart', handleTouchStart, { passive: false });
                    th.addEventListener('touchmove', handleTouchMove, { passive: false });
                    th.addEventListener('touchend', handleTouchEnd, { passive: false });
                });
            }

            function handleDragStart(e) {
                draggedColId = e.target.dataset.colId;
                draggedColType = e.target.dataset.colType;
                draggedElement = e.target;
                e.target.classList.add('dragging');
                e.dataTransfer.effectAllowed = 'move';
            }

            function handleTouchStart(e) {
                const touch = e.touches[0];
                draggedElement = e.target.closest('th[draggable="true"]');
                if (draggedElement) {
                    draggedColId = draggedElement.dataset.colId;
                    draggedColType = draggedElement.dataset.colType;
                    draggedElement.classList.add('dragging');
                }
            }

            function handleDragEnd(e) {
                e.target.classList.remove('dragging');
                document.querySelectorAll('.drag-over').forEach(el => el.classList.remove('drag-over'));
                removeDropIndicator();
                draggedColId = null;
                draggedColType = null;
                draggedElement = null;
            }

            function handleDragOver(e) {
                e.preventDefault();
                const th = e.target.closest('th[data-col-id]');
                if (!th || !canDropInPosition(th)) {
                    removeDropIndicator();
                    return;
                }
                e.dataTransfer.dropEffect = 'move';
                showDropIndicator(th, e.clientX);
            }

            function handleDragLeave(e) {
                const th = e.target.closest('th[data-col-id]');
                if (th) th.classList.remove('drag-over');
                removeDropIndicator();
            }

            function handleDrop(e) {
                e.preventDefault();
                const th = e.target.closest('th[data-col-id]');
                if (th) handleDropTarget(th);
            }

            function handleTouchMove(e) {
                if (!draggedElement) return;
                e.preventDefault();
                const touch = e.touches[0];
                const elementBelow = document.elementFromPoint(touch.clientX, touch.clientY);
                document.querySelectorAll('.drag-over').forEach(el => el.classList.remove('drag-over'));
                const th = elementBelow?.closest('th[data-col-id]');
                if (th && canDropInPosition(th)) {
                    th.classList.add('drag-over');
                    showDropIndicator(th, touch.clientX);
                } else {
                    removeDropIndicator();
                }
            }

            function handleTouchEnd(e) {
                if (!draggedElement) return;
                e.preventDefault();
                const elementBelow = document.elementFromPoint(
                    e.changedTouches[0].clientX,
                    e.changedTouches[0].clientY
                );
                const th = elementBelow?.closest('th[data-col-id]');
                if (th) handleDropTarget(th);
                draggedElement.classList.remove('dragging');
                document.querySelectorAll('.drag-over').forEach(el => el.classList.remove('drag-over'));
                removeDropIndicator();
                draggedColId = null;
                draggedColType = null;
                draggedElement = null;
            }

            function showDropIndicator(th, clientX) {
                removeDropIndicator();
                const rect = th.getBoundingClientRect();
                const isAfter = clientX > rect.left + rect.width / 2;
                const indicator = document.createElement('div');
                indicator.className = 'drop-indicator ' + (isAfter ? 'after' : 'before');
                th.style.position = 'relative';
                th.appendChild(indicator);
            }

            function removeDropIndicator() {
                document.querySelectorAll('.drop-indicator').forEach(el => el.remove());
            }

            function handleDropTarget(th) {
                th.classList.remove('drag-over');
                const targetColId = th.dataset.colId;
                const fromIndex = currentOrder.indexOf(draggedColId);
                let toIndex = currentOrder.indexOf(targetColId);
                toIndex = constrainTarget(toIndex, draggedColType !== 'category');
                if (fromIndex !== toIndex) {
                    moveColumnInOrder(fromIndex, toIndex);
                    reorderTable();
                }
            }

            function canDropInPosition(th) {
                const targetIndex = currentOrder.indexOf(th.dataset.colId);
                if (draggedColType === 'category') {
                    return targetIndex > CATEGORY_END;
                }
                return targetIndex < CATEGORY_START || targetIndex > CATEGORY_END;
            }

            function constrainTarget(targetIndex, isNonCategory) {
                if (isNonCategory) {
                    if (targetIndex >= CATEGORY_START && targetIndex <= CATEGORY_END) {
                        return CATEGORY_END + 1;
                    }
                }
                return targetIndex;
            }

            function moveColumnInOrder(fromIndex, toIndex) {
                if (draggedColType === 'category') {
                    moveCategoryGroupInOrder(fromIndex, toIndex);
                } else {
                    const [moved] = currentOrder.splice(fromIndex, 1);
                    const insertIndex = toIndex > fromIndex && toIndex > CATEGORY_END + 1 ? toIndex - 1 : toIndex;
                    currentOrder.splice(insertIndex, 0, moved);
                }
            }

            function moveCategoryGroupInOrder(fromIndex, toIndex) {
                const firstCategoryIndex = currentOrder.findIndex(col => CATEGORY_COLUMNS.includes(col));
                const categoryCount = CATEGORY_COLUMNS.length;
                
                const targetIsInCategoryRange = toIndex <= CATEGORY_END;
                if (targetIsInCategoryRange) {
                    return;
                }
                
                let adjustedToIndex = toIndex;
                if (toIndex > fromIndex) {
                    adjustedToIndex = toIndex - categoryCount;
                } else {
                    adjustedToIndex = Math.max(toIndex - categoryCount + 1, CATEGORY_END + 1);
                }
                
                const removedCategories = currentOrder.splice(firstCategoryIndex, categoryCount);
                currentOrder.splice(adjustedToIndex, 0, ...removedCategories);
            }

            function reorderTable() {
                const table = document.querySelector('table');
                if (!table) return;

                const headerCells = Array.from(table.querySelectorAll('thead th'));
                headerCells.sort((a, b) =>
                    currentOrder.indexOf(a.dataset.colId) - currentOrder.indexOf(b.dataset.colId)
                );
                const headerRow = table.querySelector('thead tr');
                headerCells.forEach(cell => headerRow.appendChild(cell));

                table.querySelectorAll('tbody tr').forEach(row => {
                    const cells = Array.from(row.querySelectorAll('td'));
                    cells.sort((a, b) =>
                        currentOrder.indexOf(a.dataset.colId) - currentOrder.indexOf(b.dataset.colId)
                    );
                    cells.forEach(cell => row.appendChild(cell));
                });
            }

            function resetColumnOrder() {
                currentOrder = [...CATEGORY_COLUMNS, 'image', 'color', 'quantity', 'name', 'partNumber'];
                reorderTable();
            }

            window.resetColumnOrder = resetColumnOrder;
        })();
    </script>
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
      msg.startsWith("No set found for") || msg.startsWith("No inventory found for")
    }
  }
}
