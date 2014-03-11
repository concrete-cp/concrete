package concrete.generator.cspompatterns

import cspom.CSPOM
import scala.collection.mutable.Queue
import cspom.compiler.ConstraintCompiler
import cspom.compiler.ConstraintCompilerNoData
import cspom.CSPOMConstraint
import cspom.variable.CSPOMConstant
import cspom.variable.BoolVariable
import cspom.compiler.Delta
import cspom.variable.FreeVariable

object RemoveAnd extends ConstraintCompilerNoData {
  def matchBool(constraint: CSPOMConstraint[_], problem: CSPOM) = constraint match {
    case CSPOMConstraint(CSPOMConstant(true), 'and, _, _) => true
    case _ => false
  }

  def compile(constraint: CSPOMConstraint[_], problem: CSPOM) = {

    require(constraint.arguments.forall(v => v.isInstanceOf[FreeVariable] || v.isInstanceOf[BoolVariable] || v.isTrue), constraint)

    problem.removeConstraint(constraint)

    Delta().removed(constraint) ++ replace[Any, Boolean](constraint.arguments.distinct, CSPOMConstant(true), problem)

  }

  def selfPropagation = false
}
