package cspfj.constraint;

import java.util.Arrays
import cspfj.problem.Variable;
import scala.annotation.tailrec

trait Removals extends Constraint {

  val removals = new Array[Int](arity)

  final override def setRemovals(p: Int, v: Int) {
    removals(p) = v
  }

  final override def fillRemovals(value: Int) {
    Arrays.fill(removals, value);
  }

  /**
   * If only one variable in the scope has been altered, its revision can be skipped
   */
  final def skipRevision(reviseCount: Int) = {
    @tailrec
    def cand(i: Int, r: Int): Int =
      if (i < 0) r
      else if (removals(i) >= reviseCount)
        if (r < 0) cand(i - 1, i)
        else -1
      else cand(i - 1, r)

    cand(arity - 1, -1)

  }

  final def varsWithRemovals(reviseCount: Int) = {
    @tailrec
    def vWR(i: Int, r: List[(Variable, Int)]): List[(Variable, Int)] =
      if (i < 0) r
      else if (removals(i) >= reviseCount) vWR(i - 1, (scope(i), i) :: r)
      else vWR(i - 1, r)

    vWR(arity - 1, Nil)

  }
  // scope.iterator.zipWithIndex.zip(removals.iterator).filter(t => t._2 >= reviseCount).map(t => t._1)

  final def modified(reviseCount: Int) = {
    @tailrec
    def mod(i: Int, r: List[Variable]): List[Variable] =
      if (i < 0) r
      else if (removals(i) >= reviseCount) mod(i - 1, scope(i) :: r)
      else mod(i - 1, r)

    mod(arity - 1, Nil)
  }
  //scope.iterator.zip(removals.iterator).filter(t => t._2 >= reviseCount).map(t => t._1)

}