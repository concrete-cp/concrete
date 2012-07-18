package cspfj.constraint.extension;

import cspfj.Variable
import scala.annotation.tailrec
import cspfj.constraint.TupleEnumerator
import java.util.Arrays
import cspfj.constraint.Constraint
import cspfj.Parameter
import cspfj.ParameterManager

object ExtensionConstraint {

  @Parameter("reduction")
  var reductionType = classOf[ExtensionConstraintArray]

  ParameterManager.register(this)

  def newExtensionConstraint(matrix: Matrix, scope: Array[Variable]) =
    matrix match {
      case m: Matrix2D => new ExtensionConstraint2D(scope, m, true)
      case m: TupleTrieSet => {
//        println(m.trie.arity)
//        println(m.trie.size)
//        println(m.trie.nodes)
//        println("---")
        reductionType.getConstructor(classOf[Array[Variable]], classOf[TupleTrieSet]).newInstance(scope, m)
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

