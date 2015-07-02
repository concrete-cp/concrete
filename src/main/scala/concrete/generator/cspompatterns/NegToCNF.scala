package concrete.generator.cspompatterns

import cspom.CSPOM
import scala.collection.mutable.Queue
import cspom.compiler.ConstraintCompiler
import cspom.CSPOMConstraint
import cspom.variable.CSPOMVariable
import cspom.compiler.Delta
import cspom.variable.BoolVariable
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMConstant
import cspom.compiler.ConstraintCompilerNoData
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMSeq
import concrete.CSPOMDriver
import cspom.variable.BoolExpression
import cspom.variable.SimpleExpression

/**
 * Negation is converted to CNF :
 *
 * a = -b <=> (a v b) ^ (-a v -b)
 */
//  private def generateNeg(result: Variable, arg: Variable) {
//
//    addConstraint(new Disjunction(result, arg));
//    addConstraint(new Disjunction(Array(result, arg), Array(true, true)));
//
//  }
object NegToCNF extends ConstraintCompiler {

  type A = (SimpleExpression[Boolean], SimpleExpression[Boolean])

  override def constraintMatcher = {
    case CSPOMConstraint(BoolExpression(res), 'not, Seq(BoolExpression(arg)), params) =>
      (res, arg)

  }

  def compile(fc: CSPOMConstraint[_], problem: CSPOM, data: A) = {

    val (res, arg) = data

    val newConstraints = Seq(
      CSPOMDriver.clause(res, arg)(),
      CSPOMDriver.clause()(res, arg))

    replaceCtr(fc, newConstraints, problem)

  }
  def selfPropagation = false
}
