package concrete.generator.cspompatterns

import cspom.CSPOMConstraint
import cspom.compiler.{ConstraintCompiler, VariableCompiler}
import cspom.util.IntInterval
import cspom.variable.{IntExpression, SimpleExpression}

object MinDomains extends VariableCompiler('min) {

  def compiler(c: CSPOMConstraint[_]): Seq[(SimpleExpression[_], SimpleExpression[Int])] = {
    c match {
      case CSPOMConstraint(r: SimpleExpression[_], _, a: Seq[SimpleExpression[Int]], _) =>
        val ir = IntExpression.coerce(r)

        val itv = a.map(IntExpression.span)

        Seq(
          r -> ConstraintCompiler.reduceDomain(ir, IntInterval(itv.map(_.lb).min, itv.map(_.ub).min)))

      case e => throw new IllegalArgumentException(s"$e is not a valid min constraint")
    }
  }
}

object MaxDomains extends VariableCompiler('max) {

  def compiler(c: CSPOMConstraint[_]): Seq[(SimpleExpression[_], SimpleExpression[Int])] = c match {
    case CSPOMConstraint(r: SimpleExpression[_], _, a: Seq[SimpleExpression[Int]], _) =>
      val ir = IntExpression.coerce(r)

      val itv = a.map(IntExpression.span)

      Seq(
        r -> ConstraintCompiler.reduceDomain(ir, IntInterval(itv.map(_.lb).max, itv.map(_.ub).max)))

    case e => throw new IllegalArgumentException(s"$e is not a valid max constraint")
  }
}