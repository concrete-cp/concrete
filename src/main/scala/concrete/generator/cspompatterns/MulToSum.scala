package concrete.generator.cspompatterns

import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.compiler.ConstraintCompilerNoData
import cspom.compiler.Delta
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMSeq

object MulToSum extends ConstraintCompilerNoData {

  def matchBool(c: CSPOMConstraint[_], p: CSPOM) = {
    c.function == 'mul && c.arguments.count(_.isInstanceOf[CSPOMConstant[_]]) == 1
  }

  def compile(c: CSPOMConstraint[_], in: CSPOM) = {
    val Seq(v0, v1) = c.arguments

    (c.result, v0, v1) match {
      case (r, CSPOMConstant(v0), v1) =>
        replaceCtr(c, CSPOMConstraint('sum, Seq(new CSPOMSeq(Seq(r, v1)), CSPOMConstant(0)),
          c.params ++ Map("coefficients" -> Seq(-1, v0), "mode" -> "eq")), in)
      case (r, v0, CSPOMConstant(v1)) =>
        replaceCtr(c, CSPOMConstraint('sum, Seq(new CSPOMSeq(Seq(r, v0)), CSPOMConstant(0)),
          c.params ++ Map("coefficients" -> Seq(-1, v1), "mode" -> "eq")), in)
      case _ => Delta()
    }
  }

  def selfPropagation = false
}