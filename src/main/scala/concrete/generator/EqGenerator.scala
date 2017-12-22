package concrete
package generator;

import com.typesafe.scalalogging.LazyLogging
import Generator.cspom2concrete1D
import concrete.constraint.Constraint
import concrete.constraint.linear.EqCReif
import concrete.constraint.linear.EqReif
import concrete.constraint.semantic.{ClauseConstraint, Neq}
import cspom.CSPOMConstraint

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

        if (result.initDomain == BooleanDomain(a == b)) {
          Seq()
        } else if (a == b) {
          Seq(new ClauseConstraint(Array(result), Array()))
        } else {
          Seq(new ClauseConstraint(Array(), Array(result)))
        }
      case (Const(a: Int), Var(b)) => Seq(new EqCReif(result, b, a))
      case (Var(a), Const(b: Int)) => Seq(new EqCReif(result, a, b))
      case (Var(a), Var(b)) => Seq(new EqReif(result, a, b))
      case e => throw new IllegalArgumentException(s"$e contain unsupported domain types")
    }

  }

}
