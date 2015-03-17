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
import cspom.variable.BoolExpression

object BoolEq extends ConstraintCompiler {

  type A = (SimpleExpression[_], SimpleExpression[_], SimpleExpression[_])

  override def constraintMatcher = {
    case CSPOMConstraint(
      r: SimpleExpression[_], 'eq,
      Seq(a: SimpleExpression[_], b: SimpleExpression[_]), _) if (
      a.isInstanceOf[BoolVariable] || b.isInstanceOf[BoolVariable]) =>
      (r, a, b)
  }

  def compile(c: CSPOMConstraint[_], problem: CSPOM, data: A) = {

    val (r, a, b) = data

    val nr = BoolExpression(r)
    val na = BoolExpression(a)
    val nb = BoolExpression(b)

    replace(Seq(a), na, problem) ++
      replace(Seq(b), nb, problem) ++
      replace(Seq(r), nr, problem) ++
      replaceCtr(c, Seq(
        CSPOMDriver.clause(Seq(nb), Seq(nr, na)),
        CSPOMDriver.clause(Seq(na), Seq(nr, na)),
        CSPOMDriver.clause(Seq(nr, na, nb), Seq()),
        CSPOMDriver.clause(Seq(nr), Seq(na, nb))), problem)

  }

  def selfPropagation = true

}