package concrete.generator.cspompatterns

import cspom.compiler.ConstraintCompilerNoData
import cspom.CSPOMConstraint
import cspom.CSPOM
import cspom.compiler.Delta

/**
 * If the given constraint is an all-different or neq constraint, remove it
 * if it is subsumed by another difference constraint.
 *
 * @param constraint
 */
object SubsumedDiff extends ConstraintCompilerNoData {

  def matchBool(constraint: CSPOMConstraint, problem: CSPOM) =
    AllDiff.ALLDIFF_CONSTRAINT(constraint) && haveSubsumingConstraint(constraint, problem)

  def compile(constraint: CSPOMConstraint, problem: CSPOM) = {
    problem.removeConstraint(constraint)
    Delta().removed(constraint)
  }

  private def haveSubsumingConstraint(
    constraint: CSPOMConstraint, problem: CSPOM) = {
    val smallestDegree = constraint.fullScope.minBy(problem.constraints(_).size)

    problem.constraints(smallestDegree).exists(
      c => c != constraint && AllDiff.DIFF_CONSTRAINT(c) && constraint.fullScope.toSet.subsetOf(c.fullScope.toSet))
  }

  def selfPropagation = false
}