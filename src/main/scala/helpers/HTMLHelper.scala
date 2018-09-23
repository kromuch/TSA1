package helpers

import scala.xml._
import scala.xml.XML._

object HTMLHelper {
  def plotsListHTML(plotNames: List[String]): NodeSeq = {
    Seq(<html>
    <h1>Available plots:</h1><br></br>
    {plotNames.map{name =>
      <h2><a href={s"/plots/$name"} target="_blank">{name}</a></h2><br></br>
    }}
    </html>)
  }
}
