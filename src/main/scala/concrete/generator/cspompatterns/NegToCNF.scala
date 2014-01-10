package concrete.generator.cspompatterns

import cspom.CSPOM
import scala.collection.mutable.Queue
import cspom.compiler.ConstraintCompiler
import cspom.CSPOMConstraint
import cspom.variable.CSPOMVariable
import cspom.compiler.Delta
import cspom.variable.BoolVariable
import cspom.variable.CSPOMTrue
import cspom.variable.CSPOMConstant
import cspom.compiler.ConstraintCompilerNoData
import cspom.variable.BoolExpression
import cspom.variable.CSPOMFalse
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

  type A = (BoolExpression, BoolExpression)

  def mtch = constraintMatch andThen {
    case CSPOMConstraint(res: BoolExpression, 'not, Seq(arg: BoolExpression), params) =>
      (res, arg)

  }

  def compile(fc: CSPOMConstraint, problem: CSPOM, data: A) = {

    val (res, arg) = data
    problem.removeConstraint(fc)

    Delta().removed(fc).added(problem.ctr(
      new CSPOMConstraint(CSPOMTrue, 'or, Seq(res, arg), fc.params))).added(problem.ctr(
      new CSPOMConstraint(CSPOMTrue, 'or, Seq(res, arg), fc.params + ("revsign" -> Seq(true, true)))))

  }

}
