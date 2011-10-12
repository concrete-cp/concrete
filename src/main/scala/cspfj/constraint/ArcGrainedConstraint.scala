package cspfj.constraint;

import java.util.Arrays;

import cspfj.problem.Variable;

trait ArcGrainedConstraint extends Constraint {

  val removals = new Array[Int](arity)

  final def setRemovals(p: Int, v: Int) {
    removals(p) = v
  }

  final def fillRemovals(value: Int) {
    Arrays.fill(removals, value);
  }

  final def hasNoRemovals(value: Int) = removals.forall(_ < value)

  /**
   * If only one variable in the scope has been altered, its revision can be skipped
   */
  final def skipRevision(reviseCount: Int) = {
    val candidates = removals.zipWithIndex.filter(_._1 >= reviseCount).take(2)

    if (candidates.size == 1) {
      candidates.first._2
    } else {
      -1
    }
  }

  final def varsWithRemovals(reviseCount: Int) =
    (0 to arity).iterator.filter(removals(_) >= reviseCount).map(scope(_))

}