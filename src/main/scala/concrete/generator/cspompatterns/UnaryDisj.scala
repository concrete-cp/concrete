package concrete.generator.cspompatterns

import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.compiler.ConstraintCompiler
import cspom.variable.CSPOMConstant
import cspom.variable.SimpleExpression
import cspom.compiler.Delta

/**
 * Unary disjunction transformed to equality
 */
object UnaryOr extends ConstraintCompiler {

  type A = (Boolean, SimpleExpression[_])

  override def constraintMatcher = {
    case CSPOMConstraint(CSPOMConstant(res: Boolean), 'or, Seq(arg: SimpleExpression[_]), params) =>
      (res, arg)
  }

  def compile(fc: CSPOMConstraint[_], problem: CSPOM, data: A) = {

    val (res, arg) = data

    val Seq(constant: Boolean) = fc.params.getOrElse("revsign", Seq(false))

    replaceCtr(fc, Seq(), problem) ++ replace(Seq(arg), CSPOMConstant(constant ^ res), problem)

  }

  def selfPropagation = false
}
