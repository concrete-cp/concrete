package concrete.generator.cspompatterns

import cspom.compiler.ConstraintCompilerNoData
import cspom.CSPOMConstraint
import cspom.CSPOM
import cspom.compiler.ConstraintCompiler
import cspom.variable.SimpleExpression
import cspom.variable.CSPOMSeq
import cspom.variable.CSPOMConstant

object MinMax extends ConstraintCompiler {

  type A = Symbol

  override def constraintMatcher = {
    case CSPOMConstraint(CSPOMConstant(_), 'max, _, _) => 'ge
    case CSPOMConstraint(CSPOMConstant(_), 'min, _, _) => 'le
  }

  def compile(c: CSPOMConstraint[_], p: CSPOM, mode: Symbol) = {
    val r = c.result
    val eq = c.arguments.map {
      case v: SimpleExpression[_] => CSPOMConstraint(mode, Seq(r, v))
      case _                      => ???
    }

    replaceCtr(c, CSPOMConstraint('in, Seq(r, CSPOMSeq(c.arguments: _*))) +: eq, p)
  }

  def selfPropagation = false
}