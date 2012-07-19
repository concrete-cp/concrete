package cspfj.constraint.extension;

import cspfj.Variable
import scala.annotation.tailrec
import cspfj.constraint.TupleEnumerator
import java.util.Arrays
import cspfj.constraint.Constraint
import cspfj.Parameter
import cspfj.ParameterManager

abstract class ExtensionConstraint(
  scope: Array[Variable],
  private var _matrix: Matrix,
  var shared: Boolean) extends Constraint(scope) {

  override def checkIndices(t: Array[Int]) = _matrix.check(t)
  def checkValues(t: Array[Int]) = throw new UnsupportedOperationException
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

