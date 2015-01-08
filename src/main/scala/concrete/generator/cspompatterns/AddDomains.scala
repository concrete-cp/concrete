package concrete.generator.cspompatterns

import cspom.CSPOMConstraint
import cspom.compiler.VariableCompiler
import cspom.variable.IntExpression.implicits.arithmetics
import cspom.variable.IntExpression.implicits.ranges
import cspom.variable.IntExpression
import cspom.variable.SimpleExpression

object AddDomains extends VariableCompiler('add) {

  def compiler(c: CSPOMConstraint[_]) = c match {
    case CSPOMConstraint(r: SimpleExpression[_], _, Seq(i0: SimpleExpression[_], i1: SimpleExpression[_]), _) =>
      val ir = IntExpression(r)
      val ii0 = IntExpression(i0)
      val ii1 = IntExpression(i1)
      Map(
        r -> reduceDomain(ir, ii0 + ii1),
        i0 -> reduceDomain(ii0, ir - ii1),
        i1 -> reduceDomain(ii1, ir - ii0))
  }
}