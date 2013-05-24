package concrete.constraint.extension;

import concrete.Variable
import scala.annotation.tailrec
import concrete.constraint.TupleEnumerator
import java.util.Arrays
import concrete.constraint.Constraint
import concrete.Parameter
import concrete.ParameterManager

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

