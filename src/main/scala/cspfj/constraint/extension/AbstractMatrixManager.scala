package cspfj.constraint.extension;

import java.util.Arrays
import cspfj.problem.Variable;
import scala.annotation.tailrec

abstract class AbstractMatrixManager(
  private var scope: Array[Variable],
  private var _matrix: Matrix,
  private var shared: Boolean,
  private var tuple: Array[Int]) extends MatrixManager {

  def matrix = _matrix

  private val domainSize = scope map (_.dom.maxSize)

  private val arity = scope.size

  private var nbInitConflicts: Array[Array[Long]] = null

  private var nbMaxConflicts: Array[Long] = null

  private def countConflicts() {
    nbInitConflicts = nbConflicts();
    if (nbInitConflicts != null) {
      nbMaxConflicts = new Array(arity);
      updateMaxConflicts();
    }
  }

  final def set(tuple: Array[Int], status: Boolean): Boolean = {
    if (matrix.check(tuple) == status) {
      return false;
    }
    if (shared) {
      unshareMatrix();
    }
    matrix.set(tuple, status);
    if (!status) {
      addConflict(tuple);
    }
    return true;
  }

  override final def removeTuple(tuple: Array[Int]) = set(tuple, false)

  final def isTrue(tuple: Array[Int]) = matrix.check(tuple)

  def deepCopy(variables: Array[Variable], tuple: Array[Int]) = {
    val clone = this.copy
    clone.tuple = tuple;
    clone.scope = variables;
    clone;
  }

  def copy = super.clone.asInstanceOf[AbstractMatrixManager]

  def check = matrix.check(tuple)

  private def firstT() {
    for (p <- tuple.indices) {
      tuple(p) = scope(p).dom.maxSize - 1
    }
  }

  @tailrec
  private def nextTR(pos: Int): Boolean = {
    if (pos < 0) {
      false
    } else {
      tuple(pos) -= 1
      if (tuple(pos) >= 0) {
        true
      } else {
        tuple(pos) = scope(pos).dom.maxSize - 1
        nextTR(pos - 1)
      }
    }
  }

  private def nextT() = nextTR(arity - 1)

  final def intersect(matrix2: Matrix) {
    firstT()
    do {
      if (!matrix2.check(tuple)) set(tuple, false)
    } while (nextT())
  }

  protected def unshareMatrix() = {
    if (!shared) {
      matrix;
    } else {
      shared = false;
      _matrix = matrix.copy
      matrix
    }
  }

  final def getType = getClass.getSimpleName + " w/ " + matrix.getClass.getSimpleName

  private def updateMaxConflicts() {
    for (p <- nbMaxConflicts.indices) {
      nbMaxConflicts(p) = nbInitConflicts(p).max
    }
  }

  def supportCondition(position: Int): Boolean = {
    if (nbMaxConflicts == null) {
      countConflicts();
    }
    getOtherSize(position) > nbMaxConflicts(position);
  }

  @tailrec
  private def getOtherSizeR(position: Int, i: Int, acc: Long): Long = {
    if (i < 0) {
      acc
    } else {
      if (i != position) {
        val dSize = scope(i).dom.size
        if (dSize > Long.MaxValue / dSize) {
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

      firstT();
      do {
        if (!check) {
          for (p <- tuple.indices) {
            nbInitConflicts(p)(tuple(p)) += 1;
          }
        }
      } while (nextT());
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

  override def toString = getClass().getSimpleName() + "\n" + matrix.toString();

}
