package cspfj.constraint.extension;

import java.util.Arrays;

final class MatrixGeneral(sizes: Seq[Int], val initialState: Boolean) extends Matrix with Cloneable {
  require(sizes.map(BigInt(_)).product <= Int.MaxValue)
  
  var matrix = Array.fill(sizes.product)(initialState)

  var skip = sizes.scan(1)(_ * _).init.toArray

  var empty = !initialState

  def check(tuple: Array[Int]) = matrix(matrixIndex(tuple))

  def set(tuple: Array[Int], status: Boolean) {
    matrix(matrixIndex(tuple)) = status
    empty = false
  }

  private def matrixIndex(tuple: Array[Int]) = {
    //(tuple, skip).zipped.map((t, s) => t * s).sum
    var s = 0;
    var i = tuple.length - 1;
    while (i >= 0) {
      s += tuple(i) * skip(i)
      i -= 1
    }
    s
  }

  def copy = {
    val clone = super.clone.asInstanceOf[MatrixGeneral]
    clone.matrix = matrix.clone
    clone
  }

  def isEmpty = empty
}
