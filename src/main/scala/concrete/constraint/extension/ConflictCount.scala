package concrete.constraint.extension

import java.util.Arrays
import scala.Array.canBuildFrom
import scala.annotation.tailrec
import concrete.constraint.TupleEnumerator
import concrete.Variable
import concrete.Domain
import concrete.ProblemState

abstract class ConflictCount(
  scope: Array[Variable],
  _matrix: Matrix,
  shared: Boolean)
  extends ExtensionConstraint(scope, _matrix, shared) with TupleEnumerator {

  def supportCondition(ps: ProblemState, position: Int): Boolean = {
    if (applicable && nbMaxConflicts == null) {
      countConflicts(ps);
    }
    applicable &&
      getOtherSize(ps, position) > nbMaxConflicts(position)
  }

  private var nbInitConflicts: Array[Array[Long]] = null

  private var offsets: Array[Int] = null

  private var nbMaxConflicts: Array[Long] = null

  private var applicable = true

  private def countConflicts(ps: ProblemState) {
    nbConflicts(ps) match {
      case Some((o, c)) =>
        nbInitConflicts = c
        offsets = o
        nbMaxConflicts = new Array(arity);
        updateMaxConflicts()
      case None => applicable = false
    }

  }

  private def updateMaxConflicts() {
    for (p <- nbMaxConflicts.indices) {
      nbMaxConflicts(p) = nbInitConflicts(p).max
    }
  }

  @tailrec
  private def getOtherSizeR(ps: ProblemState, position: Int, i: Int, acc: Long): Long = {
    if (i < 0) { acc }
    else if (i == position) { getOtherSizeR(ps, position, i - 1, acc) }
    else {
      val dSize = ps.dom(scope(i)).size
      if (acc > Long.MaxValue / dSize) {
        -1
      } else {
        getOtherSizeR(ps, position, i - 1, acc * dSize)
      }
    }
  }

  private def getOtherSize(ps: ProblemState, position: Int) = getOtherSizeR(ps, position, arity - 1, 1)

  private def nbConflicts(ps: ProblemState): Option[(Array[Int], Array[Array[Long]])] = {
    val size = cardSize(ps)
    if (size < 0) {
      None
    } else {
      val offsets = ps.domains(scope).map(_.head).toArray

      val nbInitConflicts = ps.domains(scope).map(
        d => new Array[Long](d.last - d.head + 1))
        .toArray

      matrix match {
        case tupleSet: TupleTrieSet =>
          if (tupleSet.initialContent) {

            for (tuple <- tupleSet; p <- tuple.indices) nbInitConflicts(p)(tuple(p) - offsets(p)) += 1;

          } else {

            for (p <- nbInitConflicts.indices) Arrays.fill(nbInitConflicts(p), getOtherSize(ps, p))

            for (tuple <- tupleSet; p <- tuple.indices) nbInitConflicts(p)(tuple(p) - offsets(p)) -= 1

          }

        case _ =>
          for (tuple <- tuples(ps) if (!check(tuple)); p <- tuple.indices) {
            nbInitConflicts(p)(tuple(p) - offsets(p)) += 1;
          }
      }

      Some((offsets, nbInitConflicts))
    }
  }

  final def addConflict(tuple: Array[Int]) {
    if (nbInitConflicts != null) {
      for (p <- tuple.indices) {
        nbInitConflicts(p)(tuple(p) - offsets(p)) += 1
        if (nbInitConflicts(p)(tuple(p) - offsets(p)) > nbMaxConflicts(p)) {
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
