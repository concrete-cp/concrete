package concrete.generator.cspompatterns

import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.compiler.ConstraintCompiler
import cspom.variable.CSPOMConstant
import cspom.variable.SimpleExpression
import cspom.compiler.Delta
import cspom.variable.CSPOMSeq
import cspom.variable.CSPOMExpression

/**
 * Unary disjunction transformed to equality
 */
object UnaryClause extends ConstraintCompiler {

  type A = (Boolean, CSPOMExpression[_])

  override def constraintMatcher = {
    case CSPOMConstraint(CSPOMConstant(true), 'clause, Seq(positive: CSPOMSeq[_], negative: CSPOMSeq[_]), params) if positive.length + negative.length == 1 =>
      if (positive.length == 1) {
        (true, positive.head)
      } else if (negative.length == 1) {
        (false, negative.head)
      } else {
        throw new IllegalStateException
      }
  }

  def compile(fc: CSPOMConstraint[_], problem: CSPOM, data: A) = {

    val (res, arg) = data

    removeCtr(fc, problem) ++ replace(arg, CSPOMConstant(res), problem)

  }

  def selfPropagation = false
}
