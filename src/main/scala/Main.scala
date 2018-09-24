import java.io.File

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{ContentTypes, HttpEntity}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.ActorMaterializer
import vegas._
import vegas.render.WindowRenderer._
import PlotsHandler._
import akka.util.Timeout
import akka.http.scaladsl.marshallers.xml.ScalaXmlSupport._

import scala.concurrent.{ExecutionContextExecutor, Future}

object Main extends App with InitHelper {

//  println(labConfig.toString)

  val plot = Vegas.layered("Country Pop").
    withData(
      Seq(
        Map("country" -> "USA", "population" -> 314, "averageHeight" -> 5),
        Map("country" -> "UK", "population" -> 64, "averageHeight" -> 6),
        Map("country" -> "DK", "population" -> 80, "averageHeight" -> 8)
      )
    ).withLayers(
    Layer().
      mark(Line).
      encodeX("country", Nom).
      encodeY("population", Quant)
//      .encodeColor("population", Quant)
//      Layer().
//        mark(Area).
//        encodeX("country", Nom).
////        encodeY("population", Quant).
//        encodeY("averageHeight", Quant)
    )

  val testPlot = plot.html.pageHTML()

  plotsHandler.tell(Add(Plot("test", testPlot)), ActorRef.noSender)

  val route: Route =
    path("") {
      get {
        val f = akka.pattern.ask(plotsHandler, GetPlotsList).mapTo[List[String]]
        onSuccess(f){list =>
          complete(helpers.HTMLHelper.plotsListHTML(list))
        }
      }
    } ~ pathPrefix("plots" / Segment) { name =>
      val f = akka.pattern.ask(plotsHandler, GetPlotByName(java.net.URLDecoder.decode(name))).mapTo[Option[HTML]]
      onSuccess(f) {optionHTML =>
        complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, optionHTML.getOrElse(noHTMLByName)))
      }
    }

  val host = "localhost"
  val port = 8080

  val bindingFuture: Future[Http.ServerBinding] = Http().bindAndHandle(route, host, port)
    .andThen{
      case scala.util.Success(value) => println(s"Server online at http://$host:$port/")
      case scala.util.Failure(exception) => println("Server offline")
    }
}