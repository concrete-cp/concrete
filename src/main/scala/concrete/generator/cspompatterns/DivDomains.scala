package concrete.generator.cspompatterns

import cspom.CSPOMConstraint
import cspom.compiler.VariableCompiler
import cspom.variable.IntExpression.implicits.arithmetics
import cspom.variable.IntExpression.implicits.ranges
import cspom.variable.IntExpression
import cspom.variable.SimpleExpression

object DivDomains extends VariableCompiler('div) {

  // r = i0/i1
  // -> i0 = r * i1
  def compiler(c: CSPOMConstraint[_]) = c match {
    case CSPOMConstraint(r: SimpleExpression[_], _, Seq(i0: SimpleExpression[_], i1: SimpleExpression[_]), _) =>
      val ir = IntExpression.coerce(r)
      val ii0 = IntExpression.coerce(i0)
      val ii1 = IntExpression.coerce(i1)
      try {
        Seq(
          r -> reduceDomain(ir, ii0 / ii1))
      } catch {
        case e: ArithmeticException =>
          logger.warn(s"$e when filtering $c")
          Seq()
      }
  }
}