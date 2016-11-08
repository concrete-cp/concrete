package concrete
package generator;

import com.typesafe.scalalogging.LazyLogging

import Generator.cspom2concrete1D
import concrete.constraint.Constraint
import concrete.constraint.ReifiedConstraint
import concrete.constraint.linear.Eq
import concrete.constraint.linear.EqACFast
import concrete.constraint.linear.LinearEq
import concrete.constraint.semantic.Neq
import cspom.CSPOMConstraint
import cspom.UNSATException
import concrete.constraint.linear.EqReif
import concrete.constraint.linear.SumMode
import concrete.constraint.linear.SumEQ
import concrete.constraint.linear.Linear
import concrete.constraint.linear.SumNE
import concrete.constraint.linear.EqCReif

final class EqGenerator(pm: ParameterManager) extends Generator with LazyLogging {

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
      case (Const(a: Int), Var(b)) => Seq(new EqCReif(result, b, a))
      case (Var(a), Const(b: Int)) => Seq(new EqCReif(result, a, b))
      case (Var(a), Var(b)) => Seq(new EqReif(result, a, b))
      case e => throw new IllegalArgumentException(s"$e contain unsupported domain types")
    }

  }

}
