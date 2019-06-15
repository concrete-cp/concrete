package concrete.generator.cspompatterns

import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.compiler.{ConstraintCompilerNoData, Delta, Functions}
import cspom.variable.CSPOMConstant
import cspom.variable.IntVariable
import cspom.variable.SimpleExpression
import cspom.compiler.ConstraintCompiler._

object SetIn extends ConstraintCompilerNoData {

  def functions = Functions("set_in")

  def matchBool(constraint: CSPOMConstraint[_], problem: CSPOM): Boolean =
    constraint.nonReified

  def compile(constraint: CSPOMConstraint[_], problem: CSPOM): Delta = {
    val Seq(variable: SimpleExpression[_], CSPOMConstant(set: Seq[Int] @unchecked)) = constraint.arguments

    replaceCtr(constraint, Nil, problem) ++
      replace(variable, variable.intersected(IntVariable.ofSeq(set)), problem)

  }
  def selfPropagation = false
}
