package concrete.generator.cspompatterns

import cspom.CSPOMConstraint
import cspom.compiler.VariableCompiler
import cspom.util.IntervalsArithmetic.RangeArithmetics
import cspom.variable.IntVariable.arithmetics
import cspom.variable.IntVariable.intExpression
import cspom.variable.IntVariable.ranges
import cspom.variable.SimpleExpression

object AbsDiffDomains extends VariableCompiler('absdiff) {

  def compiler(c: CSPOMConstraint[_]) = c match {
    case CSPOMConstraint(ir: SimpleExpression[_], _, Seq(ii0: SimpleExpression[_], ii1: SimpleExpression[_]), _) =>
      val r = intExpression(ir)
      val i0 = intExpression(ii0)
      val i1 = intExpression(ii1)
      Map(
        ir -> reduceDomain(r, (i0 - i1).abs),
        ii0 -> reduceDomain(i0, (i1 + r) ++ (i1 - r)),
        ii1 -> reduceDomain(i1, (i0 + r) ++ (i0 - r)))

    case _ => throw new IllegalArgumentException
  }
}