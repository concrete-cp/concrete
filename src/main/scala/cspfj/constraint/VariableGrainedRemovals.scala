package cspfj.constraint;

import java.util.Arrays
import cspfj.problem.Variable;
import scala.annotation.tailrec

trait VariableGrainedRemovals extends Constraint {

  val removals = new Array[Int](arity)

  final override def setRemovals(p: Int, v: Int) {
    removals(p) = v
  }

  final override def fillRemovals(value: Int) {
    Arrays.fill(removals, value);
  }

  final override def hasNoRemovals(value: Int) = removals.forall(_ < value)

  /**
   * If only one variable in the scope has been altered, its revision can be skipped
   */
  final def skipRevision(reviseCount: Int) = {
    @tailrec
    def cand(i: Int, r: List[Int]): List[Int] =
      if (i < 0 || r.size > 1) r
      else if (removals(i) >= reviseCount) cand(i - 1, i :: r)
      else cand(i - 1, r)

    val candidates = cand(arity - 1, Nil)
    //removals.toStream.zipWithIndex.filter(_._1 >= reviseCount).take(2)

    assert(candidates.size > 0)

    candidates match {
      case List(v) => v
      case _ => -1
    }

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