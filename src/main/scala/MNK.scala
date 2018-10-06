import breeze.linalg.{DenseMatrix, DenseVector, inv}
import vegas._

object MNK {
  def mnk(X: DenseMatrix[Double], Y: DenseVector[Double]): DenseVector[Double] = {
    val Xt = X.t
    val t = (Xt * X) \ Xt
//    println(s"MNK debug: Y size: ${Y.length} Xt cols: ${t.cols}")
    t*DenseVector(Y.data.takeRight(t.cols))
  }

//  def constructTeta(as: List[Double], bs: List[Double]): DenseVector[Double] = {
//    DenseVector(List(as, List(1.0), bs).flatten.toArray)
//  }

//  def constructX(Y: List[Double], V: List[Double], porAvt: Int,
//                 porKovz: Int): DenseMatrix[Double] = {
//    val min = math.max(porAvt, porKovz)
//    val max = math.min(Y.length, V.length)
//    val list = (min to max).map{k =>
//      val ys = (1 to porAvt).map(i => Y(k-i)).toList
//      val vses = (0 to porKovz).map(j => V(k-j)).toList
//      List(List(1.0), ys, vses).flatten
//    }.toList
//    DenseMatrix(list.map(_.toArray):_*)
//  }

  def constructX(Y: List[Double], V: List[Double], porAvt: Int,
                 porKovz: Int): DenseMatrix[Double] = {
    val min = math.max(porAvt, porKovz)
    val max = math.min(Y.length, V.length) - 1
    val list = (min to max).map{k =>
      val ys = (1 to porAvt).map(i => Y(k-i)).toList
      val vses = (0 to porKovz).map(j => V(k-j)).toList
      List(List(1.0), ys, vses).flatten
    }.toList
    DenseMatrix(list.map(_.toArray):_*)
  }

}
