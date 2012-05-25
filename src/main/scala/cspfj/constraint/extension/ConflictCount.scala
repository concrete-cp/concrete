package cspfj.constraint.extension
import scala.annotation.tailrec
import cspfj.Variable
import cspfj.constraint.TupleEnumerator
import java.util.Arrays

abstract class ConflictCount(
  scope: Array[Variable],
  _matrix: Matrix,
  shared: Boolean)
  extends ExtensionConstraint(scope: Array[Variable], _matrix, shared) with TupleEnumerator {

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
    if (i < 0) acc
    else if (i == position) getOtherSizeR(position, i - 1, acc)
    else {
      val dSize = scope(i).dom.size
      if (acc > Long.MaxValue / dSize) {
        -1
      } else {
        getOtherSizeR(position, i - 1, acc * dSize)
      }
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
      if (size > Long.MaxValue / v.dom.size) {
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

  final def set(tuple: Array[Int], status: Boolean) =
    if (matrix.check(tuple) == status) false
    else {
      unshareMatrix();
      matrix.set(tuple, status);
      if (!status) {
        addConflict(tuple);
      }
      true;
    }

}