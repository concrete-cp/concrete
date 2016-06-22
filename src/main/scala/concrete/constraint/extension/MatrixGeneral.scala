package concrete.constraint.extension;

import com.typesafe.scalalogging.LazyLogging

final class MatrixGeneral(sizes: Array[Int], offsets: Array[Int], initialState: Boolean) extends Matrix with LazyLogging {

  val size: Int = {
    val nbValues = sizes.foldLeft(BigInt(1))(_ * _)
    require(nbValues.isValidInt)
    nbValues.toInt
  }

  private var matrix = {
    Array.fill(size)(initialState)
  }

  private val skip = new Array[Int](sizes.length - 1)

  skip(0) = sizes(0)
  for (i <- 1 until skip.length) {
    skip(i) = skip(i - 1) * sizes(i)
  }

  private var empty = !initialState

  private def matrixIndex(tuple: Array[Int]): Int = {
    var index = tuple(0) - offsets(0)
    for (i <- 0 until skip.length) {
      index += skip(i) * (tuple(i + 1) - offsets(i + 1))
    }
    index
  }

  override def check(tuple: Array[Int]) = matrix(matrixIndex(tuple))

  override def set(tuple: Seq[Int], status: Boolean): Unit = {
    matrix(matrixIndex(tuple.toArray)) = status
  }

  def copy = {
    val matrixG = new MatrixGeneral(sizes, offsets, initialState)
    matrixG.matrix = this.matrix.clone
    matrixG
  }

  override def toString =
    s"sizes: ${sizes.mkString("[", ", ", "]")}, offsets: ${offsets.mkString("[", ", ", "]")}"

  def isEmpty = empty

  def allowed: Iterator[Array[Int]] = {
    ???
    matrix.indices.iterator.filter(matrix).map { i =>
      Array(i)
    }
  }

}
