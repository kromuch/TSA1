import breeze.linalg.{DenseMatrix, DenseVector}

import scala.annotation.tailrec

object RMNK {
  def calculatePsina(Y: DenseVector[Double], V: DenseVector[Double], k: Int, n: Int, m: Int): DenseVector[Double] = {
    val newY = (1 to n).map(i => Y(k-i)).toList
    val newV = (0 to m).map(j => V(k-j)).toList
    DenseVector((List(1.0) ++ newY ++ newV).toArray)
  }

  def calculateGamma(PKMinus1: DenseMatrix[Double], alpha: Double, psinaK: DenseVector[Double]): DenseVector[Double] = {
    val part1 = PKMinus1*psinaK
    val part2 = psinaK.t * PKMinus1 * psinaK
    val part3 = 1 / alpha
    part1 / (part3 + part2)
  }

  def calculateNextP(PKMinus1: DenseMatrix[Double], alpha: Double, psina: DenseVector[Double]): DenseMatrix[Double]  = {
    val part1 = PKMinus1*psina*psina.t*PKMinus1
    val part2 = 1 / alpha
    val part3 = psina.t * PKMinus1 * psina
    PKMinus1 - (part1/(part2 + part3))
  }

  def rmnk(Y: DenseVector[Double], V: DenseVector[Double], n: Int, m: Int, kmax: Int):
  (DenseVector[Double], List[DenseVector[Double]], Double, List[Double]) = {
    val tetaVzhuh = DenseVector.zeros[Double](n + m + 2)
    val P = DenseMatrix.zeros[Double](n + m + 2, n + m + 2)
    for(i <- 0 until P.rows)
      for(j <- 0 until P.cols)
        if (i == j) P(i, j) = 300.0 else P(i, j) = 0.0
    val alpha = 0.95 / 0.98

    @tailrec
    def rec(PKMinus1: DenseMatrix[Double], tetaKMinus1: DenseVector[Double], k: Int,
            acc: List[DenseVector[Double]] = List.empty[DenseVector[Double]],
            accE: Double = 0.0,
            accYVzhuh: List[Double] = List.empty[Double]):
    (DenseVector[Double], List[DenseVector[Double]], Double, List[Double]) = {
      if (k > kmax)
        (tetaKMinus1, acc, accE, accYVzhuh)
      else {
        val psina = calculatePsina(Y, V, k, n, m)
        val gamma = calculateGamma(PKMinus1, k, psina)
        val yVzhuh = tetaKMinus1.t*psina
        val e = Y(k) - yVzhuh
        val teta = tetaKMinus1 + gamma*e
        val P = calculateNextP(PKMinus1, alpha, psina)
        rec(P, teta, k + 1, acc :+ teta, accE + e * e, accYVzhuh :+ yVzhuh)
      }
    }

    rec(P, tetaVzhuh, k = 1)
  }
}
