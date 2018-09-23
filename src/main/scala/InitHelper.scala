import java.io.File

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.stream.ActorMaterializer
import akka.util.Timeout

import scala.concurrent.duration._
import scala.concurrent.ExecutionContextExecutor

trait InitHelper {
  implicit val system: ActorSystem = ActorSystem()
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  // needed for the future flatMap/onComplete in the end
  implicit val executionContext: ExecutionContextExecutor = system.dispatcher
  implicit val timeout: Timeout = 5.seconds


  val plotsHandler: ActorRef = system.actorOf(Props(new PlotsHandler))

  val noHTMLByName = "No any plot by this name was found"

  val configPath: String = "C:\\Users\\kromu\\Documents\\MEGAsync\\Навчання\\4 курс\\" +
    "АЧР\\Л1\\ATS_Lab_01_new\\ATS_Lab_01_new\\Test+\\"

  val labConfig = new LabConfig(new File(configPath + "test.txt"),
    new File(configPath + "arkc.txt"),
    new File(configPath + "y.txt"),
    new File(configPath + "v.txt")
  )
}
