package concrete.generator.cspompatterns

import scala.reflect.runtime.universe

import concrete.CSPOMDriver
import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.compiler.ConstraintCompiler
import cspom.variable.BoolExpression
import cspom.variable.SimpleExpression

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