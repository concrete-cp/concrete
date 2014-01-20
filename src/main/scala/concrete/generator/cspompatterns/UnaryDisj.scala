package concrete.generator.cspompatterns

import cspom.CSPOM
import scala.collection.mutable.Queue
import cspom.compiler.ConstraintCompiler
import cspom.CSPOMConstraint
import cspom.variable.CSPOMVariable
import cspom.compiler.Delta
import cspom.variable.BoolVariable
import cspom.variable.CSPOMTrue
import cspom.variable.CSPOMConstant
import cspom.compiler.ConstraintCompilerNoData
import cspom.variable.BoolExpression
import cspom.variable.CSPOMFalse
/**
 * Unary disjunction transformed to equality
 */
object UnaryOr extends ConstraintCompiler {

  type A = BoolExpression

  override def constraintMatcher = {
    case CSPOMConstraint(CSPOMTrue, 'or, Seq(arg: BoolVariable), params) =>
      arg
  }

  def compile(fc: CSPOMConstraint, problem: CSPOM, arg: A) = {

    problem.removeConstraint(fc)

    val delta = Delta().removed(fc)

    val Seq(constant: Boolean) = fc.params.getOrElse("revsign", Seq(false))

    if (constant) {
      delta.added(problem.ctr(new CSPOMConstraint(CSPOMTrue, 'eq, Seq(arg, CSPOMFalse))))
    } else {
      delta.added(problem.ctr(new CSPOMConstraint(CSPOMTrue, 'eq, Seq(arg, CSPOMTrue))))
    }

  }

}
