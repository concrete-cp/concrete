package concrete.generator.cspompatterns

import concrete.CSPOMDriver.CSPOMIntExpressionOperations
import cspom.{CSPOM, CSPOMConstraint}
import cspom.compiler.{ConstraintCompilerNoData, Delta}
import cspom.variable.{CSPOMConstant, IntExpression}

object MulToSum extends ConstraintCompilerNoData {

  def matchBool(c: CSPOMConstraint[_], p: CSPOM) = {
    c.function == 'mul && c.arguments.collect { case CSPOMConstant(_) => true }.size == 1
  }

  def compile(c: CSPOMConstraint[_], in: CSPOM) = {
    val Seq(v0, v1) = c.arguments

    (c.result, v0, v1) match {
      case (IntExpression(r), CSPOMConstant(v0: Int), IntExpression(v1)) =>
        replaceCtr(c, -1 *: r + v0 *: v1 === 0, in)

      case (IntExpression(r), IntExpression(v0), CSPOMConstant(v1: Int)) =>
        replaceCtr(c, -1 *: r + v1 *: v0 === 0, in)

      case _ => Delta()
    }
  }

  def selfPropagation = false
}