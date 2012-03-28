package cspfj.constraint.extension;

import cspfj.problem.Variable
import scala.annotation.tailrec
import cspfj.constraint.TupleEnumerator
import java.util.Arrays
import cspfj.constraint.Constraint

object ExtensionConstraint {

  def newExtensionConstraint(matrix: Matrix, scope: Array[Variable]) =
    matrix match {
      case m: Matrix2D => new ExtensionConstraint2D(scope, m, true)
      case m: TupleSet => new ExtensionConstraintDynamic(scope, m, true)
      case m => new ExtensionConstraintGeneral(m, true, scope)
    }

}

abstract class ExtensionConstraint(scope: Array[Variable]) extends Constraint(scope) {
  //def matrix: Matrix
  def checkValues(t: Array[Int]) = throw new UnsupportedOperationException
  def removeTuples(base: Array[Int]): Int
  def removeTuple(tuple: Array[Int]): Boolean
}

abstract class ConflictCount(scope: Array[Variable]) extends ExtensionConstraint(scope: Array[Variable]) with TupleEnumerator {

  def matrix: Matrix

  def supportCondition(position: Int): Boolean = {
    if (nbMaxConflicts == null) {
      countConflicts();
    }
    getOtherSize(position) > nbMaxConflicts(position);
  }

  private var nbInitConflicts: Array[Array[Long]] = null

  private var nbMaxConflicts: Array[Long] = null

  private def countConflicts() {
    nbInitConflicts = nbConflicts();
    if (nbInitConflicts != null) {
      nbMaxConflicts = new Array(arity);
      updateMaxConflicts();
    }
  }

  private def updateMaxConflicts() {
    for (p <- nbMaxConflicts.indices) {
      nbMaxConflicts(p) = nbInitConflicts(p).max
    }
  }

  @tailrec
  private def getOtherSizeR(position: Int, i: Int, acc: Long): Long = {
    if (i < 0) {
      acc
    } else {
      if (i != position) {
        val dSize = scope(i).dom.size
        if (acc > Long.MaxValue / dSize) {
          -1
        } else {
          getOtherSizeR(position, i - 1, acc * dSize)
        }
      } else getOtherSizeR(position, i - 1, acc)
    }
  }

  private def getOtherSize(position: Int) = getOtherSizeR(position, arity - 1, 1)

  private def nbConflicts(): Array[Array[Long]] = {
    val size = currentSize
    if (size < 0) {
      return null;
    }

    val nbInitConflicts = scope map {
      v => new Array[Long](v.dom.maxSize)
    }

    if (matrix.isInstanceOf[TupleSet]) {

      val tupleSet = matrix.asInstanceOf[TupleSet];
      val initialContent = tupleSet.initialContent
      if (!initialContent) {
        for (p <- nbInitConflicts.indices) {
          Arrays.fill(nbInitConflicts(p), getOtherSize(p))
        }
      }

      for (tuple <- tupleSet) {
        if (!initialContent) {
          for (p <- tuple.indices) {
            nbInitConflicts(p)(tuple(p)) -= 1;
          }
        } else {
          for (p <- tuple.indices) {
            nbInitConflicts(p)(tuple(p)) += 1;
          }
        }
      }

    } else if (!matrix.isEmpty) {

      for (tuple <- tuples() if (!checkIndices(tuple)); p <- tuple.indices) {
        nbInitConflicts(p)(tuple(p)) += 1;
      }
    }

    nbInitConflicts
  }

  protected final def currentSize: Long = {
    var size = 1
    for (v <- scope) {
      if (size > Integer.MAX_VALUE / v.dom.size) {
        return -1;
      }
      size *= v.dom.size
    }
    size
  }

  final def addConflict(tuple: Array[Int]) {
    if (nbInitConflicts != null) {
      for (p <- tuple.indices) {
        nbInitConflicts(p)(tuple(p)) += 1
        if (nbInitConflicts(p)(tuple(p)) > nbMaxConflicts(p)) {
          nbMaxConflicts(p) += 1
        }
      }
    }
  }

}
