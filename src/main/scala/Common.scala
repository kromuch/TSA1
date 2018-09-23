import scala.annotation.tailrec

object Common {
  val rand = new scala.util.Random()

  def gaussian: Double = {
    rand.nextGaussian() * 0.05
  }

  def calculateY(as: List[Double], bses: List[Double], maxCount: Int): List[Double] = {
    val porAvt = as.length - 1
    val porKovz = bses.length
    val initYs = (1 to porAvt).map(_ => gaussian).toList
    val initVses = (1 to porKovz).map(_ => gaussian).toList

    @tailrec
    def rec(newYs: List[Double], newVses: List[Double], k: Int): List[Double] = {
      if (k >= maxCount) //TODO: чекнути потім
        newYs.reverse
      else {
        val newV = gaussian
        val newY = as.head + as.tail.zipWithIndex.foldLeft(0.0){case (zero, (a, index)) =>
            zero + a * newYs(index)
        } + newV + bses.zipWithIndex.foldLeft(0.0){case (zero, (b, index)) =>
          zero + b * newVses(index)
        }
        rec(newY :: newYs, newV :: newVses, k + 1)
      }
    }

    rec(initYs, initVses, porAvt)
  }
}
