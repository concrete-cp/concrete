package concrete.generator.cspompatterns

import cspom.CSPOMConstraint
import cspom.compiler.VariableCompiler
import cspom.util.{Finite, Infinitable, IntInterval, RangeSet}
import cspom.variable.IntExpression.implicits.{arithmetics, ranges}
import cspom.variable.{IntExpression, SimpleExpression}

object ModDomains extends VariableCompiler('mod) {

  // r = i0 % i1

  def compiler(c: CSPOMConstraint[_]) = c match {
    case CSPOMConstraint(r: SimpleExpression[_], _, Seq(i0: SimpleExpression[_], i1: SimpleExpression[_]), _) =>
      val ir = IntExpression.coerce(r)
      val ii0 = IntExpression.coerce(i0)
      val ii1 = IntExpression.coerce(i1)

      try {
        val ub = ii1.abs.upperBound
        val result: RangeSet[Infinitable] = RangeSet(IntInterval(-ub, ub))
        Seq(
          r -> reduceDomain(ir, result))
      } catch {
        case e: ArithmeticException =>
          logger.warn(s"$e when filtering $c")
          Seq()
      }
  }
}