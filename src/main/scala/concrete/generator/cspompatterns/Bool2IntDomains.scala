package concrete.generator.cspompatterns

import cspom.CSPOMConstraint
import cspom.VariableNames
import cspom.compiler.VariableCompiler
import cspom.util.IntInterval._
import cspom.util.IntInterval
import cspom.util.IntervalsArithmetic._
import cspom.util.RangeSet
import cspom.variable.BoolVariable
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMExpression
import cspom.variable.IntVariable
import cspom.variable.IntVariable.arithmetics
import cspom.variable.IntVariable.intExpression
import cspom.variable.IntVariable.ranges
import cspom.variable.SimpleExpression

object Bool2IntDomains extends VariableCompiler('bool2int) {

  def compiler(c: CSPOMConstraint[_]) = c match {
    case CSPOMConstraint(CSPOMConstant(true), _, Seq(i0: SimpleExpression[_], i1: SimpleExpression[_]), params) =>

      val b = BoolVariable.boolExpression(i0)
      val i = intExpression(i1)

      val ii = b match {
        case CSPOMConstant(false) => RangeSet(IntInterval.singleton(0))
        case CSPOMConstant(true) => RangeSet(IntInterval.singleton(1))
        case _: BoolVariable => RangeSet(IntInterval(0, 1))
      }

      val bb = if (b.isInstanceOf[BoolVariable]) {
        i match {
          case CSPOMConstant(0) => CSPOMConstant(false)
          case CSPOMConstant(1) => CSPOMConstant(true)
          case _ => b
        }
      } else { b }

      Map(i0 -> bb, i1 -> reduceDomain(i, ii))

  }
}