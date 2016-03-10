package concrete.constraint.extension;

import concrete.Variable
import concrete.constraint.Constraint

abstract class ExtensionConstraint(scope: Array[Variable]) extends Constraint(scope) {

  def matrix: Matrix

  override def check(t: Array[Int]) = matrix.check(t)
  //  def checkValues(t: Array[Int]) = {
  //    val indices = (t, scope).zipped.map {
  //      (value, variable) => variable.dom.index(value)
  //    }
  //    checkIndices(indices)
  //  }
  def removeTuples(base: Array[Int]): Int
  def removeTuple(tuple: Array[Int]): Boolean

}

