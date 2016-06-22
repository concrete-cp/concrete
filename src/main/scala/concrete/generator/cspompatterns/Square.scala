package concrete.generator.cspompatterns

import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.compiler.ConstraintCompiler
import cspom.variable.CSPOMExpression

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
