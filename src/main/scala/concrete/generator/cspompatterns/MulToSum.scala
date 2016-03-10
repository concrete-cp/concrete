package concrete.generator.cspompatterns

import concrete.CSPOMDriver
import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.compiler.ConstraintCompilerNoData
import cspom.compiler.Delta
import cspom.variable.CSPOMConstant
import cspom.variable.IntExpression

object MulToSum extends ConstraintCompilerNoData {

  def matchBool(c: CSPOMConstraint[_], p: CSPOM) = {
    c.function == 'mul && c.arguments.count(PartialFunction.cond(_) { case CSPOMConstant(_) => true }) == 1
  }

  def compile(c: CSPOMConstraint[_], in: CSPOM) = {
    val Seq(v0, v1) = c.arguments

    (c.result, v0, v1) match {
      case (IntExpression(r), CSPOMConstant(v0: Int), IntExpression(v1)) =>
        replaceCtr(c, CSPOMDriver.linear(Seq((-1, r), (v0, v1)), "eq", 0), in)
        
      case (IntExpression(r), IntExpression(v0), CSPOMConstant(v1: Int)) =>
        replaceCtr(c, CSPOMDriver.linear(Seq((-1, r), (v1, v0)), "eq", 0), in)
        
      case _ => Delta()
    }
  }

  def selfPropagation = false
}