package concrete.generator.cspompatterns

import cspom.CSPOM
import scala.collection.mutable.Queue
import cspom.compiler.ConstraintCompiler
import cspom.compiler.ConstraintCompilerNoData
import cspom.CSPOMConstraint
import cspom.variable.CSPOMTrue
import cspom.variable.BoolVariable
import cspom.compiler.Delta

object RemoveAnd extends ConstraintCompilerNoData {
  def matchBool(constraint: CSPOMConstraint, problem: CSPOM) =
    constraint.function == 'and && constraint.result == CSPOMTrue

  def compile(constraint: CSPOMConstraint, problem: CSPOM) = {

    require(constraint.scope.forall(v => v.isInstanceOf[BoolVariable] || v == CSPOMTrue))

    problem.removeConstraint(constraint)
    replaceVars(constraint.scope.toSeq, CSPOMTrue, problem)

  }
}
