import java.io.{BufferedWriter, File, FileWriter}

import Main.plotsHandler
import PlotsHandler.{Add, Plot}
import akka.actor.ActorRef
import breeze.linalg.{DenseMatrix, DenseVector}
import vegas.DSL.LayerSpecBuilder
import vegas._

import scala.io.Source
import scala.util.Try

class LabConfig(file: File, arkcFile: File, yFile: File, vFile: File) {
  import Numeric.Implicits._

  def mean[T: Numeric](xs: Iterable[T]): Double = xs.sum.toDouble / xs.size

  def variance[T: Numeric](xs: Iterable[T]): Double = {
    val avg = mean(xs)

    xs.map(_.toDouble).map(a => math.pow(a - avg, 2)).sum / xs.size
  }

  private val linesList: List[String] = Source.fromFile(file).getLines().toList

  private val a0: Double = Try(linesList.head.split("=")(1).toDouble).getOrElse(0.0)
  private val a1: Double = Try(linesList(1).split("=")(1).toDouble).getOrElse(0.0)
  private val a2: Double = Try(linesList(2).split("=")(1).toDouble).getOrElse(0.0)
  private val a3: Double = Try(linesList(3).split("=")(1).toDouble).getOrElse(0.0)
  private val b1: Double = Try(linesList(4).split("=")(1).toDouble).getOrElse(0.0)
  private val b2: Double = Try(linesList(5).split("=")(1).toDouble).getOrElse(0.0)
  private val b3: Double = Try(linesList(6).split("=")(1).toDouble).getOrElse(0.0)

  val as = List(a0, a1, a2, a3)
  val bses = List(1.0, b1, b2, b3)

  val arkc: List[(Int, Int)] = Source.fromFile(arkcFile).getLines().map { line =>
    val sp = line.split("\t")
    sp(0).toInt -> sp(1).toInt
  }.toList

  val vList: List[Double] = Source.fromFile(vFile).getLines().map(_.toDouble).toList
  val vDenseVector: DenseVector[Double] = DenseVector(vList.toArray)
  val yList: List[Double] = Source.fromFile(yFile).getLines().map(_.toDouble).toList

  val N: Int = math.min(yList.length, vList.length)

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

//  val generatedY: List[Double] = Common.calculateY(as, bses, maxCount = 100)
//
//  val plotYGenerated: LayerSpecBuilder = Vegas.layered("Country Pop").
//    withData(
//      generatedY.zip(yList).zipWithIndex.map{case ((gen, ff), index) =>
//        Map("gen" -> gen, "ff" -> ff, "index" -> index)
//      }
//    ).withLayers(
//    Layer().
//      mark(Point).
//      encodeY("gen", Quant).
//      encodeX("index", Nom),
//    Layer().
//      mark(Line).
//      encodeY("ff", Quant).
//      encodeX("index", Nom)
//  )
//
//  plotsHandler.tell(Add(Plot("Y generated", plotYGenerated.html.pageHTML())), ActorRef.noSender)

  val devFromFile: Double = variance(yList)
  println("================================================")

  val mnkYWriter = new BufferedWriter(new FileWriter("debugMNK_Y.txt"))
  Common.calculateY(coefs = List(a0, a1, a2, a3, 1.0, b1), n = 100, 3, 1, vList).foreach(x => mnkYWriter.write(s"$x\n"))
  mnkYWriter.close()

  val additionalDebugWriter = new BufferedWriter(new FileWriter(s"debugC.txt"))
  additionalDebugWriter.write(s"p\tq\tMNK e\tdevMNK\tIKA MNK\tRMNK e\tdevRMNK\tIKA RMNK\n")
  arkc/*List(3 -> 1)*/.foreach{case (p, q) =>
    val newAs = as.take(p + 1)
    val newBses = bses.take(q + 1)
    val coefs: List[Double] = newAs ++ newBses
    val yForMNK = Common.calculateY(coefs = coefs, n = 100, p, q, vList)
    val mnkX: DenseMatrix[Double] = MNK.constructX(yForMNK, vList, p, q)
    println(s"MNK debug: X size: ${mnkX.rows} x ${mnkX.cols}")
    val mnk: DenseVector[Double] = MNK.mnk(mnkX, DenseVector(yForMNK.toArray))

//    val debugWriter = new BufferedWriter(new FileWriter(s"debugX_${p}_$q.txt"))
//    for(i <- 0 until mnkX.rows) {
//      for(j <- 0 until mnkX.cols)
//        debugWriter.write(s"${mnkX(i, j)}\t")
//      debugWriter.write("\n")
//    }
//
//    debugWriter.close()


    val mnkData = mnk.data.toList
    val mnkAs = mnkData.take(p+1)
    val mnkBses = mnkData.takeRight(q)
    println(s"Calculated teta [MNK]_${p}_$q ${mnkData.mkString("\t")}")
    println(s"Coefs_${p}_$q ${coefs.mkString("\t")}")
    println(s"Calculated teta [MNK]_${p}_$q:")

    val generatedYMNK = Common.calculateY(mnkData, 100, p, q, vList)

    val mnkE = generatedYMNK.zip(yForMNK).foldLeft(0.0){case (zero, (a,b)) =>
        zero + math.pow(a-b, 2)
    }

    mnkAs.zipWithIndex.foreach{case (a, index) =>
      println(s"a$index: $a")
    }

    mnkBses.zipWithIndex.foreach{case (b, index) =>
      println(s"b${index+1}: $b")
    }

    println("---------------------------------------------------------------------")
    val yDenseVector: DenseVector[Double] = DenseVector(yForMNK.toArray)

    val (rmnk, debug, rmnkEFFF, yVzhuhs) = RMNK.rmnk(yDenseVector, vDenseVector, p, q, yForMNK.length - 1)


    val devFromRMNK: Double = variance(yVzhuhs)
    val devFromMNK: Double = variance(generatedYMNK)

    println(s"Calculated teta [RMNK]_${p}_$q:")
    val generatedYRMNK = Common.calculateY(rmnk.data.toList, 100, p, q, vList)
    val rmnkE = generatedYRMNK.zip(yForMNK).foldLeft(0.0){case (zero, (a,b)) =>
      zero + math.pow(a-b, 2)
    }



    val rmnkData = rmnk.data
    val rmnkAs = rmnkData.take(p+1)
    val rmnkBses = rmnkData.takeRight(q)

    rmnkAs.zipWithIndex.foreach{case (a, index) =>
      println(s"a$index: $a")
    }

    rmnkBses.zipWithIndex.foreach{case (b, index) =>
      println(s"b${index+1}: $b")
    }

    println(s"Сума похибок РМНК_${p}_$q: $rmnkE")

    println(s"${p}_$q:\t devFromFile: $devFromFile\tdevFromRMNK: $devFromRMNK\tВідношення: ${devFromRMNK/devFromFile}")

    val IKA_RMNK = N * math.log(rmnkE) + 2 * (p + q + 1)
    val IKA_MNK = N * math.log(mnkE) + 2 * (p + q + 1)

    println(s"IKA_RMNK${p}_$q: $IKA_RMNK")
    println(s"IKA_MNK${p}_$q: $IKA_MNK")

    val debugWriter2 = new BufferedWriter(new FileWriter(s"debugTeta_${p}_$q.txt"))
    debug.foreach{deb =>
      debugWriter2.write(s"${deb.data.mkString("\t")}")
      debugWriter2.newLine()
    }
    debugWriter2.close()
    additionalDebugWriter.write(s"$p\t$q\t$mnkE\t$devFromMNK\t$IKA_MNK\t$rmnkE\t$devFromRMNK\t$IKA_RMNK\n")
    println("================================================")
  }
  additionalDebugWriter.close()

  override def toString: String =
    s"LabConfig:\n" +
      s"a0: $a0\n" +
      s"a1: $a1\n" +
      s"a2: $a2\n" +
      s"a3: $a3\n" +
      s"b1: $b1\n" +
      s"ARKC: ${arkc.length} elements"
}
