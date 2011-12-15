package cspfj.constraint;

import java.util.Arrays;

import cspfj.problem.Variable;

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
    val candidates = removals.toStream.zipWithIndex.filter(_._1 >= reviseCount).take(2)

    assert(candidates.size > 0)
    
    if (candidates.size == 1) {
      candidates.head._2
    } else {
      -1
    }
  }

  final def varsWithRemovals(reviseCount: Int): Iterator[(Variable, Int)] =
    scope.iterator.zipWithIndex.zip(removals.iterator).filter(t => t._2 >= reviseCount).map(t => t._1)

}