package concrete.generator.cspompatterns

import cspom.CSPOMConstraint
import cspom.compiler.VariableCompiler

import cspom.variable.IntExpression
import cspom.variable.SimpleExpression
import cspom.util.IntInterval

object MinDomains extends VariableCompiler('min) {

  def compiler(c: CSPOMConstraint[_]) = {
    c match {

      case CSPOMConstraint(r: SimpleExpression[_], _, a: Seq[SimpleExpression[Int]], _) =>
        val ir = IntExpression.coerce(r)

        val itv = a.map(IntExpression.span)

        Seq(
          r -> reduceDomain(ir, IntInterval(itv.map(_.lb).min, itv.map(_.ub).min)))
    }
  }
}

object MaxDomains extends VariableCompiler('max) {

  def compiler(c: CSPOMConstraint[_]) = c match {
    case CSPOMConstraint(r: SimpleExpression[_], _, a: Seq[SimpleExpression[Int]], _) =>
      val ir = IntExpression.coerce(r)

      val itv = a.map(IntExpression.span)

      Seq(
        r -> reduceDomain(ir, IntInterval(itv.map(_.lb).max, itv.map(_.ub).max)))
  }
}