package concrete.constraint.extension

import java.util

import concrete.Domain
import concrete.constraint.TupleEnumerator

import scala.annotation.tailrec

trait ConflictCount
  extends ExtensionConstraint with TupleEnumerator {

  private var nbInitConflicts: Array[Array[Long]] = _

  //def matrix_=(m: Matrix): Unit
  private var offsets: Array[Int] = _
  private var nbMaxConflicts: Array[Long] = _
  private var applicable = true

  def matrix: Matrix

  def supportCondition(doms: Array[Domain], position: Int): Boolean = {
    applicable && {
      if (nbMaxConflicts eq null) {
        countConflicts(doms)
      }

      getOtherSize(doms, position) > nbMaxConflicts(position)
    }
  }

  final def set(tuple: Array[Int], status: Boolean): Boolean =
    if (matrix.check(tuple) == status) {
      false
    }
    else {
      matrix.set(tuple, status)
      if (!status) {
        addConflict(tuple)
      }
      true
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

  private def countConflicts(doms: Array[Domain]) {
    nbConflicts(doms) match {
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
  private def getOtherSizeR(doms: Array[Domain], position: Int, i: Int, acc: Long): Long = {
    if (i < 0) {
      acc
    }
    else if (i == position) {
      getOtherSizeR(doms, position, i - 1, acc)
    }
    else {
      val dSize = doms(i).size
      if (acc > Long.MaxValue / dSize) {
        -1
      } else {
        getOtherSizeR(doms, position, i - 1, acc * dSize)
      }
    }
  }

  private def getOtherSize(doms: Array[Domain], position: Int) = getOtherSizeR(doms, position, arity - 1, 1)

  @tailrec
  private def card(doms: Array[Domain], p: Int = arity - 1, size: Int = 1): Int =
    if (p < 0) {
      size
    } else {
      val s = doms(p).size
      if (size > Int.MaxValue / s) {
        -1
      } else {
        card(doms, p - 1, size * s)
      }
    }

  private def nbConflicts(doms: Array[Domain]): Option[(Array[Int], Array[Array[Long]])] = {
    val size = card(doms)
    if (size < 0) {
      None
    } else {
      val offsets = doms.map(_.head)

      val nbInitConflicts = doms.map { d =>
        new Array[Long](d.last - d.head + 1)
      }

      matrix match {
        case tupleSet: MDDMatrix =>
          if (tupleSet.initialContent) {

            for {
              tuple <- tupleSet.mdd
              p <- tuple.indices
              if doms(p).contains(tuple(p))
            } {
              nbInitConflicts(p)(tuple(p) - offsets(p)) += 1
            }

          } else {

            for (p <- nbInitConflicts.indices) util.Arrays.fill(nbInitConflicts(p), getOtherSize(doms, p))

            for (tuple <- tupleSet.mdd; p <- tuple.indices) nbInitConflicts(p)(tuple(p) - offsets(p)) -= 1

          }

        case _ =>
          for (tuple <- tuples(doms) if !check(tuple); p <- tuple.indices) {
            nbInitConflicts(p)(tuple(p) - offsets(p)) += 1
          }
      }

      Some((offsets, nbInitConflicts))
    }
  }

}
