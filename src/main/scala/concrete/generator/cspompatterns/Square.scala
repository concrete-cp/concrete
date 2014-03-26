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
import cspom.variable.SimpleExpression

/**
 * Converts a = b × b to a = b² 
 */
object Square extends ConstraintCompiler {

  type A = (CSPOMExpression[_], CSPOMExpression[_])

  override def constraintMatcher = {
    case CSPOMConstraint(r, 'mul, Seq(a, b), _) if (a eq b) => (r, a)
  }

  def compile(fc: CSPOMConstraint[_], problem: CSPOM, data: A) = {

    val (r, a) = data

    replaceCtr(fc, CSPOMConstraint(r, 'sq, Seq(a), fc.params), problem)

  }

  def selfPropagation = false

}
