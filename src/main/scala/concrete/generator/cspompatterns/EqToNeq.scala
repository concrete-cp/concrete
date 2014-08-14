package concrete.generator.cspompatterns

import cspom.CSPOMConstraint
import cspom.compiler.VariableCompiler
import cspom.variable.IntVariable.arithmetics
import cspom.variable.IntVariable.ranges
import cspom.variable.SimpleExpression
import cspom.util.IntervalsArithmetic._
import cspom.variable.BoolVariable
import cspom.variable.IntVariable.intExpression
import cspom.variable.CSPOMConstant
import cspom.util.IntInterval._
import cspom.variable.IntVariable
import cspom.util.IntInterval
import cspom.variable.CSPOMExpression
import cspom.VariableNames
import cspom.compiler.ConstraintCompiler
import cspom.compiler.ConstraintCompilerNoData
import cspom.CSPOM

object EqToNeq extends ConstraintCompiler {

  type A = Symbol

  override def constraintMatcher = {
    case CSPOMConstraint(r: SimpleExpression[_], func,
      _, params) if r.isFalse && (
      func == 'eq || func == 'ne) && !params.contains("offset") && !params.contains("neg") =>
      func
  }

  def compile(c: CSPOMConstraint[_], problem: CSPOM, data: A) = {
    val opposite = data match {
      case 'ne => CSPOMConstraint('eq, c.arguments, c.params)
      case 'eq => CSPOMConstraint('ne, c.arguments, c.params)
    }
    
    replaceCtr(c, opposite, problem)
  }
  
  def selfPropagation = false
  
}