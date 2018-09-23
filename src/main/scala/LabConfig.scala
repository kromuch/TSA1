import java.io.{BufferedWriter, File, FileWriter}

import Main.plotsHandler
import PlotsHandler.{Add, Plot}
import akka.actor.ActorRef
import breeze.linalg.{DenseMatrix, DenseVector}
import vegas.DSL.LayerSpecBuilder
import vegas._

import scala.io.Source

class LabConfig(file: File, arkcFile: File, yFile: File, vFile: File) {
  private val linesList: List[String] = Source.fromFile(file).getLines().toList

  private val a0: Double = linesList.head.split("=")(1).toDouble
  private val a1: Double = linesList(1).split("=")(1).toDouble
  private val a2: Double = linesList(2).split("=")(1).toDouble
  private val a3: Double = linesList(3).split("=")(1).toDouble
  private val b1: Double = linesList(4).split("=")(1).toDouble
//  private val b2: Double = linesList(5).split("=")(1).toDouble
//  private val b3: Double = linesList(6).split("=")(1).toDouble

  val as = List(a0, a1, a2, a3)
  val bses = List(b1/*, b2, b3*/)

  val arkc: List[(Double, Double)] = Source.fromFile(arkcFile).getLines().map { line =>
    val sp = line.split("\t")
    sp(0).toDouble -> sp(1).toDouble
  }.toList

  val vList: List[Double] = Source.fromFile(vFile).getLines().map(_.toDouble).toList
  val yList: List[Double] = Source.fromFile(yFile).getLines().map(_.toDouble).toList

  val plotY: LayerSpecBuilder = Vegas.layered("Country Pop").
    withData(
      yList.zipWithIndex.map{case (value, index) =>
          Map("value" -> value, "index" -> index)
      }
    ).withLayers(
    Layer().
      mark(Line).
      encodeY("value", Quant).
      encodeX("index", Nom)
  )

  plotsHandler.tell(Add(Plot("Y from File", plotY.html.pageHTML())), ActorRef.noSender)

  val plotV: LayerSpecBuilder = Vegas.layered("Country Pop").
    withData(
      vList.zipWithIndex.map{case (value, index) =>
        Map("value" -> value, "index" -> index)
      }
    ).withLayers(
    Layer().
      mark(Line).
      encodeY("value", Quant).
      encodeX("index", Nom)
  )

  plotsHandler.tell(Add(Plot("V from File", plotV.html.pageHTML())), ActorRef.noSender)

  val generatedY: List[Double] = Common.calculateY(as, bses, maxCount = 100)

  val plotYGenerated: LayerSpecBuilder = Vegas.layered("Country Pop").
    withData(
      generatedY.zip(yList).zipWithIndex.map{case ((gen, ff), index) =>
        Map("gen" -> gen, "ff" -> ff, "index" -> index)
      }
    ).withLayers(
    Layer().
      mark(Point).
      encodeY("gen", Quant).
      encodeX("index", Nom),
    Layer().
      mark(Line).
      encodeY("ff", Quant).
      encodeX("index", Nom)
  )

  plotsHandler.tell(Add(Plot("Y generated", plotYGenerated.html.pageHTML())), ActorRef.noSender)

  val mnkX: DenseMatrix[Double] = MNK.constructX(yList, vList, 3, 3)
  val mnk: DenseVector[Double] = MNK.mnk(mnkX, DenseVector(yList.toArray))

  val debugWriter = new BufferedWriter(new FileWriter("debugX.txt"))
  for(i <- 0 until mnkX.rows) {
    for(j <- 0 until mnkX.cols)
      debugWriter.write(s"${mnkX(i, j)}\t")
    debugWriter.write("\n")
  }

  debugWriter.close()

  println(s"Calculated teta: ${mnk.data.mkString("\t")}")

  override def toString: String =
    s"LabConfig:\n" +
      s"a0: $a0\n" +
      s"a1: $a1\n" +
      s"a2: $a2\n" +
      s"a3: $a3\n" +
      s"b1: $b1\n" +
      s"ARKC: ${arkc.length} elements"
}
