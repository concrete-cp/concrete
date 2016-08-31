package concrete.generator;

import com.typesafe.scalalogging.LazyLogging

import Generator.cspom2concrete1D
import concrete.BooleanDomain
import concrete.constraint.Constraint
import concrete.constraint.ReifiedConstraint
import concrete.constraint.linear.Eq
import concrete.constraint.linear.EqACFast
import concrete.constraint.linear.LinearEq
import concrete.constraint.linear.LinearNe
import concrete.constraint.semantic.Neq
import cspom.CSPOMConstraint
import cspom.UNSATException
import concrete.constraint.linear.EqReif

final object EqGenerator extends Generator with LazyLogging {

  override def genReversed(constraint: CSPOMConstraint[Boolean])(implicit variables: VarMap): Seq[Constraint] = {
    val Seq(a, b) = constraint.arguments.map(cspom2concrete1D)

    (a, b) match {
      case (Const(a: Int), Const(b: Int)) if (a != b) => Seq()
      case (Var(a), Var(b)) => Seq(new Neq(a, b))

      case _ => throw new UnsupportedOperationException(s"$constraint is not supported")
    }

  }

  override def genFunctional(funcConstraint: CSPOMConstraint[_], r: C2Conc)(implicit variables: VarMap): Seq[Constraint] = {
    val Var(result) = r
    val Seq(a, b) = funcConstraint.arguments map cspom2concrete1D

    (a, b) match {
      case (Const(a: Int), Const(b: Int)) =>
        require(result.initDomain == BooleanDomain(a == b), s"$funcConstraint is inconsistent")
        Seq()
      case (Const(a: Int), Var(b)) => Seq(
        new ReifiedConstraint(result,
          LinearEq(a, Array(1), Array(b)),
          new LinearNe(a, Array(1), Array(b))))
      case (Var(a), Const(b: Int)) => Seq(
        new ReifiedConstraint(result,
          LinearEq(b, Array(1), Array(a)),
          new LinearNe(b, Array(1), Array(a))))
      case (Var(a), Var(b)) => Seq(
        new EqReif(result, a ,b))
      case e => throw new IllegalArgumentException(s"$e contain unsupported domain types")
    }

  }

}
