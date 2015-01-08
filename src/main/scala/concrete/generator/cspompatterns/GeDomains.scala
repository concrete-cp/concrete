package concrete.generator.cspompatterns

import cspom.CSPOMConstraint
import cspom.compiler.VariableCompiler
import cspom.util.Finite
import cspom.util.IntInterval
import cspom.variable.BoolVariable.boolExpression
import cspom.variable.IntExpression.implicits.arithmetics
import cspom.variable.IntExpression
import cspom.variable.IntExpression.implicits.ranges
import cspom.variable.SimpleExpression

object GeDomains extends VariableCompiler('ge) {

  def compiler(c: CSPOMConstraint[_]) = c match {
    case CSPOMConstraint(r: SimpleExpression[_], _, Seq(i0: SimpleExpression[_], i1: SimpleExpression[_]), _) =>
      val ir = boolExpression(r)

      val ii0 = IntExpression(i0)
      val ii1 = IntExpression(i1)

      val ri0: SimpleExpression[Int] = ii1.lowerBound match {
        case Finite(l) if ir.isTrue => reduceDomain(ii0, IntInterval.atLeast(l))
        case _ => ii0
      }

      val ri1 = ii0.upperBound match {
        case Finite(u) if ir.isTrue => reduceDomain(ii1, IntInterval.atMost(u))
        case _ => ii1
      }

      Map(
        r -> ir,
        i0 -> ri0,
        i1 -> ri1)
  }
}