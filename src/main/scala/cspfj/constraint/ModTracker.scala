package cspfj.constraint

import scala.annotation.tailrec
import cspfj.problem.Variable

trait ModTracker extends Constraint {
  var lastSizes: Array[Int] = null
  var lastRecorded: Int = -1

  final def saveSizes(revCount: Int) {
    lastSizes = sizes
    lastRecorded = revCount
  }

  final def modified(revCount: Int) = {
    @tailrec
    def mod(i: Int, r: List[Variable]): List[Variable] =
      if (i < 0) r
      else if (scope(i).dom.size != lastSizes(i)) mod(i - 1, scope(i) :: r)
      else mod(i - 1, r)

    if (lastRecorded < revCount) scope.toSeq else mod(arity - 1, Nil)
  }

  /**
   * If only one variable in the scope has been altered, its revision can be skipped
   */
  final def skipRevision(revCount: Int) = {
    
    @tailrec
    def cand(i: Int, r: Int): Int =
      if (i < 0) r
      else if (scope(i).dom.size != lastSizes(i))
        if (r < 0) cand(i - 1, i)
        else -1
      else cand(i - 1, r)

    if (lastRecorded < revCount) -1 else cand(arity - 1, -1)

  }

}