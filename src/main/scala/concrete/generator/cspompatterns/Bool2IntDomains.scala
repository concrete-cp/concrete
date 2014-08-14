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
import cspom.compiler.ConstraintCompilerNoData
import cspom.CSPOM
import cspom.util.Finite

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

      val bb =
        if (ii.contains(Finite(0))) {
          require(b.contains(false))
          if (ii.contains(Finite(1))) {
            require(b.contains(true))
            b
          } else {
            CSPOMConstant(false)
          }
        } else if (ii.contains(Finite(1))) {
          require(b.contains(true))
          CSPOMConstant(true)
        } else {
          throw new AssertionError()
        }

      Map(i0 -> bb, i1 -> reduceDomain(i, ii))

  }
}

object Bool2IntIsEq extends ConstraintCompilerNoData {
  def matchBool(c: CSPOMConstraint[_], p: CSPOM) = c.function == 'bool2int
  def compile(c: CSPOMConstraint[_], p: CSPOM) = {
    val b = c.arguments(0).asInstanceOf[SimpleExpression[_]]
    val i = c.arguments(1).asInstanceOf[SimpleExpression[_]]
    require(b.searchSpace == i.searchSpace &&
      (!b.contains(false) || i.contains(0) || i.contains(false)) && (
          !b.contains(true) || i.contains(1) || i.contains(true)))

    replaceCtr(c, Nil, p) ++ replace(Seq(i), b, p)
  }
  def selfPropagation = true
}