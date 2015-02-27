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
import cspom.variable.IntVariable
import cspom.util.Interval
import cspom.util.IntInterval

/**
 * Integer division is converted to multiplication :
 *
 * a = b / c
 *
 * <=>
 *
 * b = a * c + r, with r = b % c
 */
object DivToMul extends ConstraintCompiler {

  type A = CSPOMExpression[_]

  override def constraintMatcher = {
    case CSPOMConstraint(a: CSPOMExpression[_], 'div, _, _) => a
  }

  def compile(fc: CSPOMConstraint[_], problem: CSPOM, a: CSPOMExpression[_]) = {

    val Seq(b, c) = fc.arguments

    val mul = IntVariable.free()

    val remainder = IntVariable.free()

    val c1 = CSPOMConstraint(mul, 'mul, Seq(a, c))
    val c2 = CSPOMConstraint(b, 'add, Seq(mul, remainder))
    val c3 = CSPOMConstraint(remainder, 'mod, Seq(b, c))

    replaceCtr(fc, Seq(c1, c2, c3), problem)

  }

  def selfPropagation = false

}
