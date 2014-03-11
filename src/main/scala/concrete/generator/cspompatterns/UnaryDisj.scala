package concrete.generator.cspompatterns

import cspom.CSPOM
import scala.collection.mutable.Queue
import cspom.compiler.ConstraintCompiler
import cspom.CSPOMConstraint
import cspom.variable.CSPOMVariable
import cspom.compiler.Delta
import cspom.variable.BoolVariable
import cspom.variable.CSPOMConstant
import cspom.compiler.ConstraintCompilerNoData
import cspom.variable.CSPOMExpression

/**
 * Unary disjunction transformed to equality
 */
object UnaryOr extends ConstraintCompiler {

  type A = CSPOMExpression[Boolean]

  override def constraintMatcher = {
    case CSPOMConstraint(CSPOMConstant(true), 'or, Seq(arg: BoolVariable), params) =>
      arg
  }

  def compile(fc: CSPOMConstraint[_], problem: CSPOM, arg: A) = {

    val Seq(constant: Boolean) = fc.params.getOrElse("revsign", Seq(false))

    replaceCtr(fc, CSPOMConstraint('eq, arg, CSPOMConstant(constant)), problem)

  }

  def selfPropagation = false
}
