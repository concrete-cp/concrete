package concrete.generator.cspompatterns

import cspom.compiler.ConstraintCompilerNoData
import cspom.CSPOMConstraint
import cspom.CSPOM
import cspom.compiler.ConstraintCompiler
import cspom.variable.SimpleExpression
import cspom.variable.CSPOMSeq

object MinMax extends ConstraintCompiler {

  type A = Symbol

  override def constraintMatcher = {
    case c: CSPOMConstraint[_] if c.function == 'max => 'ge
    case c: CSPOMConstraint[_] if c.function == 'min => 'le
  }

  def compile(c: CSPOMConstraint[_], p: CSPOM, mode: Symbol) = {
    val r = c.result
    val eq = c.arguments.map {
      case v: SimpleExpression[_] => CSPOMConstraint(mode, r, v)
      case _ => ???
    }

    replaceCtr(c, CSPOMConstraint('in, r, new CSPOMSeq(c.arguments)) +: eq, p)
  }

  def selfPropagation = false
}