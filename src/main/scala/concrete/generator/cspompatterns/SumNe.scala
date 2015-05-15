package concrete.generator.cspompatterns

import cspom.compiler.ConstraintCompilerNoData
import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.variable.CSPOMSeq
import cspom.variable.IntVariable
import cspom.variable.CSPOMExpression
import cspom.compiler.ConstraintCompiler
import cspom.variable.CSPOMConstant

object SumNe extends ConstraintCompiler {

  type A = Seq[CSPOMExpression[_]]

  override def mtch(c: CSPOMConstraint[_], p: CSPOM) = {
    if (c.function == 'sum && c.params.get("mode").contains("ne") && c.params.get("coefficients").exists(
      c => c == Seq(1, -1) || c == Seq(-1, 1))) {
      val Seq(CSPOMSeq(args), CSPOMConstant(result)) = c.arguments
      if (result == 0) {
        Some(args)
      } else {
        None
      }
    } else {
      None
    }

  }

  def compile(c: CSPOMConstraint[_], p: CSPOM, data: A) = {

    replaceCtr(c,
      CSPOMConstraint('ne, data, c.params - "coefficients" - "mode"), p)
  }

  def selfPropagation = false

}