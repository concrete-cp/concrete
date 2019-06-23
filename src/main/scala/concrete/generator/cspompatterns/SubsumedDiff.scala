package concrete.generator.cspompatterns

import cspom.compiler.{ConstraintCompiler, ConstraintCompilerNoData, Delta, Functions}
import cspom.CSPOMConstraint
import cspom.CSPOM

/**
  * If the given constraint is an all-different or neq constraint, remove it
  * if it is subsumed by another difference constraint.
  */
object SubsumedDiff extends ConstraintCompilerNoData {

  def functions = Functions(AllDiff.ALLDIFF_FUNCTIONS: _*)

  def matchBool(constraint: CSPOMConstraint[_], problem: CSPOM): Boolean =
    haveSubsumingConstraint(constraint, problem)

  def compile(constraint: CSPOMConstraint[_], problem: CSPOM): Delta = {
    ConstraintCompiler.removeCtr(constraint, problem)
  }

  private def haveSubsumingConstraint(constraint: CSPOMConstraint[_], problem: CSPOM): Boolean = {
    val smallestDegree = constraint.fullScope.minBy(problem.unsafeConstraints(_).size)

    problem.unsafeConstraints(smallestDegree).exists(
      c => c != constraint && AllDiff.DIFF_CONSTRAINT(c).exists(cScope => constraint.fullScope.toSet.subsetOf(cScope.toSet)))
  }

  def selfPropagation = false
}