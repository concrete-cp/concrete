package concrete.generator.cspompatterns

import cspom.CSPOMConstraint
import cspom.compiler.VariableCompiler
import cspom.util.IntInterval
import cspom.variable.BoolVariable.boolExpression
import cspom.variable.IntVariable.arithmetics
import cspom.variable.IntVariable.intExpression
import cspom.variable.IntVariable.ranges
import cspom.variable.SimpleExpression

object GtDomains extends VariableCompiler('gt) {

  def compiler(c: CSPOMConstraint[_]) = c match {
    case CSPOMConstraint(r: SimpleExpression[_], _, Seq(i0: SimpleExpression[_], i1: SimpleExpression[_]), _) =>
      val ir = boolExpression(r)
      val ii0 = intExpression(i0)
      val ii1 = intExpression(i1)
      val li1 = ii1.headInterval
      val ri0: SimpleExpression[Int] = if (ir.isTrue && li1.hasLowerBound) {
        reduceDomain(ii0, IntInterval.greaterThan(li1.lowerEndpoint))
      } else {
        ii0
      }
      val ui0 = ii0.lastInterval
      val ri1 = if (ir.isTrue && ui0.hasUpperBound) {
        reduceDomain(ii1, IntInterval.lessThan(ui0.upperEndpoint))
      } else {
        ii1
      }
      Map(
        r -> ir,
        i0 -> ri0,
        i1 -> ri1)
  }
}