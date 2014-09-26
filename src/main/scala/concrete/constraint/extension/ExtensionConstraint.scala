package concrete.constraint.extension;

import scala.annotation.tailrec

import concrete.Variable
import concrete.constraint.Constraint

abstract class ExtensionConstraint(
  scope: Array[Variable],
  private var _matrix: Matrix,
  var shared: Boolean) extends Constraint(scope) {

  override def checkIndices(t: Array[Int]) = _matrix.check(t)
  def checkValues(t: Array[Int]) = {
    val indices = (t, scope).zipped.map {
      (value, variable) => variable.dom.index(value)
    }
    checkIndices(indices)
  }
  def removeTuples(base: Array[Int]): Int
  def removeTuple(tuple: Array[Int]): Boolean
  def matrix = _matrix

  def unshareMatrix() = {
    if (shared) {
      _matrix = matrix.copy
      shared = false
    }
    _matrix
  }

}

