package cspfj.constraint.extension;

import cspfj.Variable
import scala.annotation.tailrec
import cspfj.constraint.TupleEnumerator
import java.util.Arrays
import cspfj.constraint.Constraint

object ExtensionConstraint {

  def newExtensionConstraint(matrix: Matrix, scope: Array[Variable]) =
    matrix match {
      case m: Matrix2D => new ExtensionConstraint2D(scope, m, true)
      case m: TupleSet => {
        new ExtensionConstraintDynamic(scope, m, true)
        //new ExtensionConstraintTrie(scope, TrieMap.of(m))
      }
      case m => new ExtensionConstraintGeneral(m, true, scope)
    }

}

abstract class ExtensionConstraint(
  scope: Array[Variable],
  private var _matrix: Matrix,
  var shared: Boolean) extends Constraint(scope) {

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

