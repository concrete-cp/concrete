package concrete.generator.cspompatterns

import cspom.compiler.{ConstraintCompiler, ConstraintCompilerNoData, Delta}
import cspom.CSPOMConstraint
import cspom.CSPOM

/**
 * If the given constraint is an all-different or neq constraint, remove it
 * if it is subsumed by another difference constraint.
 *
 * @param constraint
 */
object SubsumedDiff extends ConstraintCompilerNoData {

  def matchBool(constraint: CSPOMConstraint[_], problem: CSPOM) =
    AllDiff.ALLDIFF_CONSTRAINT(constraint).isDefined && haveSubsumingConstraint(constraint, problem)

  def compile(constraint: CSPOMConstraint[_], problem: CSPOM) :Delta= {
    ConstraintCompiler.removeCtr(constraint, problem)
  }

  private def haveSubsumingConstraint(
    constraint: CSPOMConstraint[_], problem: CSPOM) = {
    val smallestDegree = constraint.fullScope.minBy(problem.constraints(_).size)

    problem.constraints(smallestDegree).exists(
      c => c != constraint && AllDiff.DIFF_CONSTRAINT(c).exists(cScope => constraint.fullScope.toSet.subsetOf(cScope.toSet)))
  }

  def selfPropagation = false
}