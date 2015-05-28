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

/**
 * Reified boolean equality:
 *
 * r <-> (a <-> b)
 *
 * is
 *
 *  r -> (a \/ -b),  r -> (-a \/  b),
 * -r -> (a \/  b), -r -> (-a \/ -b)
 *
 */
object BoolEq extends ConstraintCompiler {

  type A = (SimpleExpression[_], SimpleExpression[Boolean], SimpleExpression[Boolean])

  override def constraintMatcher = {
    case CSPOMConstraint(
      r: SimpleExpression[_], 'eq,
      Seq(BoolExpression(a), BoolExpression(b)), _) =>
      (r, a, b)
  }

  def compile(c: CSPOMConstraint[_], problem: CSPOM, data: A) = {

    val (r, a, b) = data

    val nr = BoolExpression.coerce(r)

    replace(r, nr, problem) ++
      replaceCtr(c, Seq(
        CSPOMDriver.clause(b)(nr, a),
        CSPOMDriver.clause(a)(nr, b),
        CSPOMDriver.clause(nr, a, b)(),
        CSPOMDriver.clause(nr)(a, b)), problem)

  }

  def selfPropagation = true

}