package concrete.generator.cspompatterns

import cspom.CSPOMConstraint
import cspom.compiler.VariableCompiler
import cspom.util.IntInterval
import cspom.variable.IntExpression.implicits.arithmetics
import cspom.variable.IntExpression
import cspom.variable.IntExpression.implicits.ranges
import cspom.variable.SimpleExpression
import cspom.util.Finite
import cspom.variable.CSPOMConstant
import cspom.util.Infinitable
import cspom.variable.BoolExpression
import cspom.variable.CSPOMExpression

object GtDomains extends VariableCompiler('gt) {

  def compiler(c: CSPOMConstraint[_]) = c match {
    case CSPOMConstraint(r: SimpleExpression[_], _, Seq(i0: SimpleExpression[_], i1: SimpleExpression[_]), _) =>
      val iir = BoolExpression(r)
      val ii0 = IntExpression(i0)
      val ii1 = IntExpression(i1)

      import Infinitable.InfinitableOrdering
      import Ordered.orderingToOrdered

      val ir = if (ii0.lowerBound > ii1.upperBound) {
        CSPOMConstant(true)
      } else if (ii0.upperBound <= ii1.lowerBound) {
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

      val f: Map[CSPOMExpression[_], CSPOMExpression[_]] = Map(
        r -> ir,
        i0 -> ri0,
        i1 -> ri1)

      //      val filter = f.filter { case (k, v) => k != v }
      //      if (filter.nonEmpty) println(s"% $c: $filter")
      f

  }
}