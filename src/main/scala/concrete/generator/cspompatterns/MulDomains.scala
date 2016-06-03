package concrete.generator.cspompatterns

import cspom.CSPOMConstraint
import cspom.compiler.VariableCompiler
import cspom.util.IntervalsArithmetic.Arithmetics
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
        Seq(
          r -> reduceDomain(ir, ii0.span * ii1.span),
          i0 -> reduceDomain(ii0, ir.span / ii1.span),
          i1 -> reduceDomain(ii1, ir.span / ii0.span))
      } catch {
        case e: ArithmeticException =>
          logger.warn(s"$e when filtering $c")
          Seq()
      }
  }
}