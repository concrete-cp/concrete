package concrete.generator.cspompatterns

import cspom.compiler.ConstraintCompilerNoData
import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.variable.CSPOMSeq
import cspom.variable.IntVariable
import cspom.variable.CSPOMExpression
import cspom.compiler.ConstraintCompiler
import cspom.variable.CSPOMConstant

object SumEq extends ConstraintCompiler {

  type A = (CSPOMExpression[_], Seq[CSPOMExpression[_]])

  override def mtch(c: CSPOMConstraint[_], p: CSPOM) = c match {
    case CSPOMConstraint(r @ CSPOMConstant(true), 'sum, Seq(CSPOMSeq(args), CSPOMConstant(0)), p) if (p.get("mode").contains("eq") && p.get("coefficients").exists(
      c => c == Seq(1, -1) || c == Seq(-1, 1))) =>
      Some(r, args)
    case _ => None
  }

  def compile(c: CSPOMConstraint[_], p: CSPOM, data: A) = {
    val (r, args) = data
    replaceCtr(c,
      CSPOMConstraint(r, 'eq, args, c.params - "coefficients" - "mode"), p)
  }

  def selfPropagation = false

}