package concrete.generator.cspompatterns

import cspom.compiler.ConstraintCompilerNoData
import cspom.CSPOMConstraint
import cspom.CSPOM
import cspom.compiler.ConstraintCompiler
import cspom.variable.SimpleExpression
import cspom.variable.BoolVariable
import cspom.variable.CSPOMSeq

object In extends ConstraintCompiler {

  type A = (SimpleExpression[_], SimpleExpression[_], Seq[SimpleExpression[_]])

  override def constraintMatcher = {
    case CSPOMConstraint(r: SimpleExpression[_], 'in, Seq(v: SimpleExpression[_], CSPOMSeq(in: Seq[SimpleExpression[_]], _, _)), _) =>
      (r, v, in)

  }

  def compile(c: CSPOMConstraint[_], p: CSPOM, data: A) = {

    val (r, v, in) = data
    val eq = in.map {
      case i => CSPOMConstraint(new BoolVariable(), 'eq, Seq(i, v))
    }

    val disj = CSPOMConstraint(r, 'or, eq.map(_.result))
    replaceCtr(c, disj +: eq, p)
  }

  def selfPropagation = false
}