package concrete.generator.cspompatterns

import cspom.{CSPOM, CSPOMConstraint}
import cspom.compiler.ConstraintCompiler
import cspom.compiler.ConstraintCompiler._
import cspom.variable.{CSPOMConstant, CSPOMExpression, CSPOMSeq}

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
