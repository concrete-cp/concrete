package concrete.generator.cspompatterns

import cspom.CSPOMConstraint
import cspom.compiler.VariableCompiler
import cspom.util.Finite
import cspom.util.IntInterval
import cspom.variable.IntExpression.implicits.arithmetics
import cspom.variable.IntExpression
import cspom.variable.IntExpression.implicits.ranges
import cspom.variable.SimpleExpression
import cspom.variable.CSPOMConstant
import cspom.util.Infinitable
import cspom.variable.BoolExpression
import cspom.variable.CSPOMExpression

object GeDomains extends VariableCompiler('ge) {

  def compiler(c: CSPOMConstraint[_]) = c match {
    case CSPOMConstraint(r: SimpleExpression[_], _, Seq(i0: SimpleExpression[_], i1: SimpleExpression[_]), _) =>
      val br = BoolExpression(r)

      val ii0 = IntExpression(i0)
      val ii1 = IntExpression(i1)

      val ri0: SimpleExpression[Int] = ii1.lowerBound match {
        case Finite(l) if br.isTrue => reduceDomain(ii0, IntInterval.atLeast(l))
        case _                      => ii0
      }

      val ri1 = ii0.upperBound match {
        case Finite(u) if br.isTrue => reduceDomain(ii1, IntInterval.atMost(u))
        case _                      => ii1
      }

      val o = Infinitable.InfinitableOrdering

      val rr = if (o.compare(ii0.lowerBound, ii1.upperBound) >= 0) {
        CSPOMConstant(true)
      } else if (o.compare(ii0.upperBound, ii1.lowerBound) < 0) {
        CSPOMConstant(false)
      } else {
        br
      }

      val f: Map[CSPOMExpression[_], CSPOMExpression[_]] = Map(
        r -> rr,
        i0 -> ri0,
        i1 -> ri1)

//      val filter = f.filter { case (k, v) => k != v }
//      if (filter.nonEmpty) println(s"% $c: $filter")
      f
  }
}