package concrete.generator.cspompatterns

import cspom.CSPOMConstraint
import cspom.compiler.VariableCompiler
import cspom.variable.IntExpression.implicits.arithmetics
import cspom.variable.IntExpression.implicits.ranges
import cspom.variable.IntExpression
import cspom.variable.SimpleExpression
import cspom.compiler.ConstraintCompiler
import cspom.variable.CSPOMConstant
import cspom.CSPOM

object AbsDomains extends VariableCompiler('abs) {

  def compiler(c: CSPOMConstraint[_]) = c match {
    case CSPOMConstraint(r: SimpleExpression[_], _, Seq(i: SimpleExpression[_]), _) =>
      val ir = IntExpression.coerce(r)
      val ii = IntExpression.coerce(i)
      Seq(
        r -> reduceDomain(ir, ii.abs),
        i -> reduceDomain(ii, ir ++ -ir))
  }
}

