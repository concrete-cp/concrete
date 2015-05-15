package concrete.generator.cspompatterns

import cspom.CSPOMConstraint
import cspom.compiler.VariableCompiler
import cspom.variable.IntExpression.implicits.arithmetics
import cspom.variable.IntExpression.implicits.ranges
import cspom.variable.IntExpression
import cspom.variable.SimpleExpression

object MulDomains extends VariableCompiler('mul) {

  def compiler(c: CSPOMConstraint[_]) = c match {
    case CSPOMConstraint(r: SimpleExpression[_], _, Seq(i0: SimpleExpression[_], i1: SimpleExpression[_]), _) =>
      val ir = IntExpression.coerce(r)
      val ii0 = IntExpression.coerce(i0)
      val ii1 = IntExpression.coerce(i1)
      try {
        Map(
          r -> reduceDomain(ir, ii0 * ii1),
          i0 -> reduceDomain(ii0, ir / ii1),
          i1 -> reduceDomain(ii1, ir / ii0))
      } catch {
        case e: ArithmeticException =>
          logger.warn(s"$e when filtering $c")
          Map()
      }
  }
}