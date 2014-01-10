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
import cspom.variable.IntExpression
import cspom.variable.CSPOMExpression

/**
 * Substraction is converted to addition :
 *
 * a = b - c
 *
 * <=>
 *
 * b = a + c
 */
object SubToAdd extends ConstraintCompiler {

  type A = CSPOMExpression

  def mtch = constraintMatch andThen {
    case CSPOMConstraint(a, 'sub, _, _) => a
  }

  def compile(fc: CSPOMConstraint, problem: CSPOM, a: CSPOMExpression) = {

    val Seq(b, c) = fc.arguments

    problem.removeConstraint(fc)

    Delta().removed(fc).added(problem.ctr(
      new CSPOMConstraint(b, 'add, Seq(a, c), fc.params)))

  }

}
