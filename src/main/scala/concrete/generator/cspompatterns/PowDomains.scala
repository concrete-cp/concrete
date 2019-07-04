package concrete.generator.cspompatterns

import cspom.CSPOMConstraint
import cspom.compiler.{ConstraintCompiler, VariableCompiler}
import cspom.util._
import cspom.util.IntervalsArithmetic.Arithmetics
import cspom.variable.IntExpression.implicits.ranges
import cspom.variable.{IntExpression, SimpleExpression}
import ConstraintCompiler._

object PowDomains extends VariableCompiler("int_pow") {

  def compiler(c: CSPOMConstraint[_]) = c match {
    case CSPOMConstraint(r: SimpleExpression[_], _, Seq(i0: SimpleExpression[_], i1: SimpleExpression[_]), _) =>
      val ir = IntExpression.coerce(r)
      val ii0 = IntExpression.coerce(i0)
      val ii1 = IntExpression.coerce(i1)

      val a = ii0.lowerBound.pow(ii1.lowerBound)
      val b = ii0.lowerBound.pow(ii1.upperBound)
      val c = ii0.upperBound.pow(ii1.lowerBound)
      val d = ii0.upperBound.pow(ii1.upperBound)

      val reducedR = IntInterval(Seq(a,b,c,d).min, Seq(a,b,c,d).max)

      try {
        Seq(
          r -> reduceDomain(ir, reducedR),
          i0 -> reduceDomain(ii0, ir.span / ii1.span),
          i1 -> reduceDomain(ii1, ir.span / ii0.span))
      } catch {
        case e: ArithmeticException =>
          logger.warn(s"$e when filtering $c")
          Seq()
      }
  }


}