import akka.actor.Actor

import PlotsHandler._

class PlotsHandler extends Actor {

  override def receive: Receive = handler(Map.empty[Name, HTML])

  def handler(map: Map[Name, HTML]): Receive = {
    case Add(plot) =>
      context.become(handler(map + (plot.name -> plot.html)))
    case GetPlotsList =>
      sender() ! map.keySet.toList
    case GetPlotByName(name) =>
      sender() ! map.get(name)
    case _ => println("PlotsHandler [handler] received unknown message")
  }
}

object PlotsHandler {
  type Name = String
  type HTML = String

  case class Plot(name: String, html: String)

  case class Add(plot: Plot)

  case object GetPlotsList

  case class GetPlotByName(name: String)
}
