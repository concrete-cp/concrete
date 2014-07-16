package concrete.constraint.extension

import java.util.Arrays

import scala.Array.canBuildFrom
import scala.annotation.tailrec

import concrete.constraint.TupleEnumerator
import concrete.Variable

abstract class ConflictCount(
  scope: Array[Variable],
  _matrix: Matrix,
  shared: Boolean)
  extends ExtensionConstraint(scope: Array[Variable], _matrix, shared) with TupleEnumerator {

  def supportCondition(position: Int): Boolean = {
    if (applicable && nbMaxConflicts == null) {
      countConflicts();
    }
    if (applicable) {
      getOtherSize(position) > nbMaxConflicts(position)
    } else false
  }

  private var nbInitConflicts: Array[Array[Long]] = null

  private var nbMaxConflicts: Array[Long] = null

  private var applicable = true

  private def countConflicts() {
    nbInitConflicts = nbConflicts();
    if (nbInitConflicts == null) {
      applicable = false
    } else {
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
    if (i < 0) { acc }
    else if (i == position) { getOtherSizeR(position, i - 1, acc) }
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
    val size = cardSize()
    if (size < 0) {
      null;
    } else {

      val nbInitConflicts = scope map {
        v => new Array[Long](v.dom.maxSize)
      }

      matrix match {
        case tupleSet: TupleTrieSet =>
          if (tupleSet.initialContent) {

            for (tuple <- tupleSet; p <- tuple.indices) nbInitConflicts(p)(tuple(p)) += 1;

          } else {

            for (p <- nbInitConflicts.indices) Arrays.fill(nbInitConflicts(p), getOtherSize(p))

            for (tuple <- tupleSet; p <- tuple.indices) nbInitConflicts(p)(tuple(p)) -= 1

          }

        case _ =>
          for (tuple <- tuples() if (!checkIndices(tuple)); p <- tuple.indices) {
            nbInitConflicts(p)(tuple(p)) += 1;
          }
      }

      nbInitConflicts
    }
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
    if (matrix.check(tuple) == status) { false }
    else {
      unshareMatrix();
      matrix.set(tuple, status);
      if (!status) {
        addConflict(tuple);
      }
      true;
    }

}
