import java.io.File

import scala.io.Source

class LabConfig(file: File, arkcFile: File) {
  private val linesList: List[String] = Source.fromFile(file).getLines().toList

  val a0: Double = linesList.head.split("=")(1).toDouble
  val a1: Double = linesList(1).split("=")(1).toDouble
  val a2: Double = linesList(2).split("=")(1).toDouble
  val a3: Double = linesList(3).split("=")(1).toDouble
  val b1: Double = linesList(4).split("=")(1).toDouble
  val b2: Double = linesList(5).split("=")(1).toDouble
  val b3: Double = linesList(6).split("=")(1).toDouble

  val arkc: List[(Double, Double)] = Source.fromFile(arkcFile).getLines().map { line =>
    val sp = line.split("\t")
    sp(0).toDouble -> sp(1).toDouble
  }.toList

  override def toString: String =
    s"LabConfig:\n" +
      s"a0: $a0\n" +
      s"a1: $a1\n" +
      s"a2: $a2\n" +
      s"a3: $a3\n" +
      s"b1: $b1\n" +
      s"ARKC: ${arkc.length} elements"
}
