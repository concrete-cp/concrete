package concrete.generator.cspompatterns

import cspom.CSPOMConstraint
import cspom.compiler.VariableCompiler
import cspom.variable.IntVariable.arithmetics
import cspom.variable.IntVariable.ranges
import cspom.variable.IntVariable.intExpression
import cspom.variable.SimpleExpression

object AbsDomains extends VariableCompiler('abs) {

  def compiler(c: CSPOMConstraint[_]) = c match {
    case CSPOMConstraint(r: SimpleExpression[_], _, Seq(i: SimpleExpression[_]), _) =>
      val ir = intExpression(r)
      val ii = intExpression(i)
      Map(
        r -> reduceDomain(ir, ii.abs),
        i -> reduceDomain(ii, ir ++ -ir))
  }
}