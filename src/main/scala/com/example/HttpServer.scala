package com.example

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
import akka.stream.Materializer
import akka.util.Timeout
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

object HttpServer {
  val HttpPort = 37080
  val HttpsPort = 37443

  def start(
    partsProcessor: ActorRef[PartsProcessor.Command],
    actorSystem: ActorSystem[_]
  )(implicit ec: ExecutionContext, materializer: Materializer): Future[Http.ServerBinding] = {
    val sslContext = SslContextBuilder.buildSslContext()
    val httpsConnectionContext = ConnectionContext.https(sslContext)

    val route = Routes.all(actorSystem, partsProcessor)

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
    partsProcessor: ActorRef[PartsProcessor.Command]
  )(implicit ec: ExecutionContext, materializer: Materializer): Route = {
    implicit val scheduler: akka.actor.typed.Scheduler = actorSystem.scheduler
    concat(
      pathSingleSlash(redirect("/parts-sorter.html", StatusCodes.Found)),
      get {
        path("parts-sorter.html") {
          complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, partsSorterHtml(None, Nil)))
        }
      },
      post {
        path("parts-sorter.html") {
          fileUpload("csvFile") { case (fileInfo, byteSource) =>
            val csvContent: Future[String] = byteSource.runFold("")(_ + _.utf8String)

            implicit val timeout: Timeout = Timeout(1.seconds)

            val processedParts: Future[List[MatchedPart]] = csvContent.flatMap { content =>
              val coloredParts = new CsvReader().readColoredPartsFromString(content)
              partsProcessor.ask(PartsProcessor.ProcessParts(coloredParts, _)).map {
                case PartsProcessor.ProcessedParts(parts) => parts
              }
            }

            onSuccess(processedParts) { results =>
              complete(HttpEntity(ContentTypes.`text/html(UTF-8)`,
                partsSorterHtml(Some(fileInfo.fileName), results)))
            }
          }
        }
      }
    )
  }

  def partsSorterHtml(fileName: Option[String], results: List[MatchedPart]): String = {
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
        tr:nth-child(even) {
            background-color: #f2f2f2;
        }
        .no-results {
            color: #666;
            font-style: italic;
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
        <form method="POST" action="/parts-sorter.html" enctype="multipart/form-data" id="uploadForm">
            <div>
                <label for="csvFile">Upload CSV file:</label><br>
                <input type="file" name="csvFile" id="csvFile" accept=".csv">
            </div>
        </form>
    </div>

    <div class="output-section">
        <h2>Output</h2>
        ${if (results.isEmpty) {
            """<p class="no-results">Upload a CSV file to see sorted results.</p>"""
        } else {
            s"""<table>
                <thead>
                    <tr>
                        <th>quantity</th>
                        <th>color</th>
                        <th>partNumber_input</th>
                        <th>name_input</th>
                        <th>partNumber_taxonomy</th>
                        <th>name_taxonomy</th>
                        <th>category</th>
                        <th>category2</th>
                        <th>category3</th>
                        <th>category4</th>
                    </tr>
                </thead>
                <tbody>
                    ${results.map { mp =>
                        val name_taxonomy = mp.legoPart.map(_.name).getOrElse("")
                        val partNumber_taxonomy = mp.legoPart.map(_.partNumber).getOrElse("")
                        val catNames = mp.legoPart.map(_.categories.map(_.name)).getOrElse(Nil)
                        s"""<tr>
                            <td>${mp.coloredPart.quantity}</td>
                            <td>${escapeHtml(mp.coloredPart.color)}</td>
                            <td>${escapeHtml(mp.coloredPart.partNumber)}</td>
                            <td>${escapeHtml(mp.coloredPart.name)}</td>
                            <td>${escapeHtml(partNumber_taxonomy)}</td>
                            <td>${escapeHtml(name_taxonomy)}</td>
                            <td>${escapeHtml(catNames.headOption.getOrElse(""))}</td>
                            <td>${escapeHtml(catNames.lift(1).getOrElse(""))}</td>
                            <td>${escapeHtml(catNames.lift(2).getOrElse(""))}</td>
                            <td>${escapeHtml(catNames.lift(3).getOrElse(""))}</td>
                        </tr>"""
                    }.mkString}
                </tbody>
            </table>"""
        }}
    </div>

    <script>
        document.getElementById('csvFile').addEventListener('change', function() {
            if (this.files.length > 0) {
                document.getElementById('uploadForm').submit();
            }
        });
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
}
