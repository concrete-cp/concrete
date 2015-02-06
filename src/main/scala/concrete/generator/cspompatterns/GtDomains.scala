package concrete.generator.cspompatterns

import cspom.CSPOMConstraint
import cspom.compiler.VariableCompiler
import cspom.util.IntInterval
import cspom.variable.BoolVariable.boolExpression
import cspom.variable.IntExpression.implicits.arithmetics
import cspom.variable.IntExpression
import cspom.variable.IntExpression.implicits.ranges
import cspom.variable.SimpleExpression
import cspom.util.Finite
import cspom.variable.CSPOMConstant
import cspom.util.Infinitable

object GtDomains extends VariableCompiler('gt) {

  def compiler(c: CSPOMConstraint[_]) = c match {
    case CSPOMConstraint(r: SimpleExpression[_], _, Seq(i0: SimpleExpression[_], i1: SimpleExpression[_]), _) =>
      val iir = boolExpression(r)
      val ii0 = IntExpression(i0)
      val ii1 = IntExpression(i1)

      val o = Infinitable.InfinitableOrdering

      val ir = if (o.compare(ii0.lowerBound, ii1.upperBound) > 0) {
        CSPOMConstant(true)
      } else if (o.compare(ii0.upperBound, ii1.lowerBound) <= 0) {
        CSPOMConstant(false)
      } else {
        iir
      }

      val ri0: SimpleExpression[Int] = ii1.lowerBound match {
        case Finite(l) if ir.isTrue => reduceDomain(ii0, IntInterval.atLeast(l + 1))
        case _                      => ii0
      }

      val ri1 = ii0.upperBound match {
        case Finite(u) if ir.isTrue => reduceDomain(ii1, IntInterval.atMost(u - 1))
        case _                      => ii1
      }

      Map(
        r -> ir,
        i0 -> ri0,
        i1 -> ri1)
  }
}