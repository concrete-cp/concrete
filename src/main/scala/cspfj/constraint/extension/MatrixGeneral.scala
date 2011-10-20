package cspfj.constraint.extension;

import java.util.Arrays;

final class MatrixGeneral(sizes: Seq[Int], val initialState: Boolean) extends Matrix with Cloneable {
  var matrix = new Array[Boolean](sizes.product)

  var skip = sizes.scan(1)(_ * _).init

  var empty = !initialState

  def check(tuple: Array[Int]) = matrix(matrixIndex(tuple))

  def set(tuple: Array[Int], status: Boolean) {
    matrix(matrixIndex(tuple)) = status
    empty = false
  }

  private def matrixIndex(tuple: Array[Int]) =
    (tuple, skip).zipped.map((t, s) => t * s).sum

  def copy = {
    val clone = super.clone.asInstanceOf[MatrixGeneral]
    clone.matrix = matrix.clone
    clone
  }

  def isEmpty = empty
}
