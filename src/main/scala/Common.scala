import breeze.linalg.DenseVector

import scala.annotation.tailrec

object Common {
  val rand = new scala.util.Random()

  def gaussian: Double = {
    rand.nextGaussian() * 0.05
  }

//  def calculateY(as: List[Double], bses: List[Double], maxCount: Int): List[Double] = {
//    val porAvt = as.length - 1
//    val porKovz = bses.length
//    val initYs = (1 to porAvt).map(_ => gaussian).toList
//    val initVses = (1 to porKovz).map(_ => gaussian).toList
//
//    @tailrec
//    def rec(newYs: List[Double], newVses: List[Double], k: Int): List[Double] = {
//      if (k >= maxCount)
//        newYs.reverse
//      else {
//        val newV = gaussian
//        val newY = as.head + as.tail.zipWithIndex.foldLeft(0.0){case (zero, (a, index)) =>
//            zero + a * newYs(index)
//        } + newV + bses.zipWithIndex.foldLeft(0.0){case (zero, (b, index)) =>
//          zero + b * newVses(index)
//        }
//        rec(newY :: newYs, newV :: newVses, k + 1)
//      }
//    }
//
//    rec(initYs, initVses, porAvt)
//  }

  def calculateY(coefs: List[Double], n: Int, p: Int, q: Int, VList: List[Double]): List[Double] = {
    val y = DenseVector.zeros[Double](n)
    for (i <- 0 until math.max(p, q))
      y(i) = VList(i)
    for (i <- math.max(p,q) until n) {
      val tPart1 = (for (j <- 1 to p) yield y(i-j)).toList
//      val tPart11 = y.data.take(i).takeRight(p).toList
      val tPart2 = (for (j <- 0 to q) yield VList(i - j)).toList
//      val tPart22 = VList.drop(i - 2 - q).take(q + 1)
      val temp: DenseVector[Double] = DenseVector((tPart1 ++ tPart2).toArray)
//      println(s"Temp vector: ${temp.data.mkString("\t")}")
      val coefsVector: DenseVector[Double] = DenseVector(coefs.tail.toArray)
//      println(s"Coefs vector: ${coefsVector.data.mkString("\t")}")
      y(i) = coefs(0) + temp.t * coefsVector
    }
    y.data.toList
  }
}
