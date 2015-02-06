package concrete.generator.cspompatterns

import concrete.CSPOMDriver
import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.compiler.ConstraintCompiler
import cspom.compiler.ConstraintCompilerNoData
import cspom.compiler.Delta
import cspom.compiler.VariableCompiler
import cspom.util.IntervalsArithmetic._
import cspom.variable.BoolVariable
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMExpression
import cspom.variable.IntExpression.implicits.arithmetics
import cspom.variable.IntExpression
import cspom.variable.IntExpression.implicits.ranges
import cspom.variable.SimpleExpression

object BoolEq extends ConstraintCompiler {

  type A = (SimpleExpression[_], SimpleExpression[_])

  override def constraintMatcher = {
    case CSPOMConstraint(
      CSPOMConstant(true), 'eq,
      Seq(a: SimpleExpression[_], b: SimpleExpression[_]), _) if (
      a.isInstanceOf[BoolVariable] || b.isInstanceOf[BoolVariable]) =>
      (a, b)
  }

  def compile(c: CSPOMConstraint[_], problem: CSPOM, data: A) = {

    val (a, b) = data

    val na = BoolVariable.boolExpression(a)
    val nb = BoolVariable.boolExpression(b)

    replace(Seq(a), na, problem) ++
      replace(Seq(b), nb, problem) ++
      replaceCtr(c, Seq(
        CSPOMDriver.clause(Seq(nb), Seq(na)),
        CSPOMDriver.clause(Seq(na), Seq(nb))), problem)

  }

  def selfPropagation = true

}