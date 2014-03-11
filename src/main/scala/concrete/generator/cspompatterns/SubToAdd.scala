package concrete.generator.cspompatterns

import cspom.CSPOM
import scala.collection.mutable.Queue
import cspom.compiler.ConstraintCompiler
import cspom.CSPOMConstraint
import cspom.variable.CSPOMVariable
import cspom.compiler.Delta
import cspom.variable.BoolVariable
import cspom.variable.CSPOMConstant
import cspom.compiler.ConstraintCompilerNoData
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

  type A = CSPOMExpression[Int]

  override def constraintMatcher = {
    case CSPOMConstraint(a: CSPOMExpression[Int], 'sub, _, _) => a
  }

  def compile(fc: CSPOMConstraint[_], problem: CSPOM, a: CSPOMExpression[Int]) = {

    val Seq(b, c) = fc.arguments

    replaceCtr(fc, CSPOMConstraint(b, 'add, Seq(a, c), fc.params), problem)

  }

  def selfPropagation = false

}
