package concrete.generator.cspompatterns

import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.compiler.{ConstraintCompiler, Delta, Functions}
import cspom.variable.CSPOMExpression

/**
 * Converts a = b × b to a = b² 
 */
object Square extends ConstraintCompiler {

  def functions = Functions('mul)

  type A = (CSPOMExpression[_], CSPOMExpression[_])

  override def constraintMatcher = {
    case CSPOMConstraint(r, _, Seq(a, b), _) if a eq b => (r, a)
  }

  def compile(fc: CSPOMConstraint[_], problem: CSPOM, data: A): Delta = {

    val (r, a) = data

    ConstraintCompiler.replaceCtr(fc, CSPOMConstraint(r, 'sq, Seq(a), fc.params), problem)

  }

  def selfPropagation = false

}
