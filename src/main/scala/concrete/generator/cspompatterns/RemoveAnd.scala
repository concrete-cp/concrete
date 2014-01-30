package concrete.generator.cspompatterns

import cspom.CSPOM
import scala.collection.mutable.Queue
import cspom.compiler.ConstraintCompiler
import cspom.compiler.ConstraintCompilerNoData
import cspom.CSPOMConstraint
import cspom.variable.CSPOMTrue
import cspom.variable.BoolVariable
import cspom.compiler.Delta
import cspom.variable.FreeVariable

object RemoveAnd extends ConstraintCompilerNoData {
  def matchBool(constraint: CSPOMConstraint, problem: CSPOM) =
    constraint.function == 'and && constraint.result == CSPOMTrue

  def compile(constraint: CSPOMConstraint, problem: CSPOM) = {

    require(constraint.fullScope.forall(v => v.isInstanceOf[FreeVariable] || v.isInstanceOf[BoolVariable] || v == CSPOMTrue), constraint)

    problem.removeConstraint(constraint)

    Delta().removed(constraint) ++ replace(constraint.fullScope.distinct, CSPOMTrue, problem)

  }
}
