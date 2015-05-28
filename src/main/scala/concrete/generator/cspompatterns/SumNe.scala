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

object SumNe extends ConstraintCompiler {

  type A = (CSPOMExpression[_], Seq[CSPOMExpression[_]])

  override def mtch(c: CSPOMConstraint[_], p: CSPOM) = PartialFunction.condOpt(c) {
    case CSPOMConstraint(r, 'sum, Seq(CSPOMSeq(args), CSPOMConstant(0)), p) if p.get("mode").contains("ne") && p.get("coefficients").exists(
      c => c == Seq(1, -1) || c == Seq(-1, 1)) =>
      (r, args)
  }

  def compile(c: CSPOMConstraint[_], p: CSPOM, data: A) = {
    val (r, args) = data
    val n = new BoolVariable()

    replaceCtr(
      c,
      Seq(
        CSPOMConstraint(n, 'not, Seq(r)),
        CSPOMConstraint(n, 'eq, args, c.params - "coefficients" - "mode")),
      p)
  }

  def selfPropagation = false

}