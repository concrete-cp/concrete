package concrete.constraint.extension

import java.util.Arrays
import scala.Array.canBuildFrom
import scala.annotation.tailrec
import concrete.constraint.TupleEnumerator
import concrete.Variable
import concrete.Domain

abstract class ConflictCount(
  scope: Array[Variable],
  _matrix: Matrix,
  shared: Boolean)
  extends ExtensionConstraint(scope: Array[Variable], _matrix, shared) with TupleEnumerator {

  def supportCondition(domains: IndexedSeq[Domain], position: Int): Boolean = {
    if (applicable && nbMaxConflicts == null) {
      countConflicts(domains);
    }
    applicable &&
      getOtherSize(domains, position) > nbMaxConflicts(position)
  }

  private var nbInitConflicts: Array[Array[Long]] = null

  private var offsets: Array[Int] = null

  private var nbMaxConflicts: Array[Long] = null

  private var applicable = true

  private def countConflicts(domains: IndexedSeq[Domain]) {
    nbConflicts(domains) match {
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
  private def getOtherSizeR(domains: IndexedSeq[Domain], position: Int, i: Int, acc: Long): Long = {
    if (i < 0) { acc }
    else if (i == position) { getOtherSizeR(domains, position, i - 1, acc) }
    else {
      val dSize = domains(i).size
      if (acc > Long.MaxValue / dSize) {
        -1
      } else {
        getOtherSizeR(domains, position, i - 1, acc * dSize)
      }
    }
  }

  private def getOtherSize(domains: IndexedSeq[Domain], position: Int) = getOtherSizeR(domains, position, arity - 1, 1)

  private def nbConflicts(domains: IndexedSeq[Domain]): Option[(Array[Int], Array[Array[Long]])] = {
    val size = cardSize(domains)
    if (size < 0) {
      None
    } else {
      val offsets = domains.map {
        d => d.head
      }
        .toArray

      val nbInitConflicts = domains.map {
        d => new Array[Long](d.last - d.head + 1)
      }
        .toArray

      matrix match {
        case tupleSet: TupleTrieSet =>
          if (tupleSet.initialContent) {

            for (tuple <- tupleSet; p <- tuple.indices) nbInitConflicts(p)(tuple(p) - offsets(p)) += 1;

          } else {

            for (p <- nbInitConflicts.indices) Arrays.fill(nbInitConflicts(p), getOtherSize(domains, p))

            for (tuple <- tupleSet; p <- tuple.indices) nbInitConflicts(p)(tuple(p) - offsets(p)) -= 1

          }

        case _ =>
          for (tuple <- tuples(domains) if (!check(tuple)); p <- tuple.indices) {
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
