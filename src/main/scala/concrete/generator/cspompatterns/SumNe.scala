package concrete.generator.cspompatterns

import cspom.compiler.ConstraintCompilerNoData
import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.variable.CSPOMSeq
import cspom.variable.IntVariable
import cspom.variable.CSPOMExpression
import cspom.compiler.ConstraintCompiler
import cspom.variable.CSPOMConstant
import cspom.variable.BoolExpression
import cspom.variable.BoolVariable
import cspom.variable.IntExpression

object SumNe extends ConstraintCompiler {

  type A = (CSPOMExpression[_], Seq[CSPOMExpression[_]])

  override def mtch(c: CSPOMConstraint[_], p: CSPOM) = PartialFunction.condOpt(c) {
    case CSPOMConstraint(r, 'sum, Seq(IntExpression.constSeq(coefs), CSPOMSeq(args), CSPOMConstant(0)), p) if p.get("mode").contains("ne") && (
      coefs == Seq(1, -1) || coefs == Seq(-1, 1)) =>
      (r, args)
  }

  def compile(c: CSPOMConstraint[_], p: CSPOM, data: A) = {
    val (r, args) = data
    val n = new BoolVariable()

    replaceCtr(
      c,
      Seq(
        CSPOMConstraint(n)('not)(r),
        CSPOMConstraint(n)('eq)(args: _*) withParams c.params - "mode"),
      p)
  }

  def selfPropagation = false

}