package concrete.generator.constraint;

import concrete.constraint.semantic.Abs
import concrete.{ Variable, Problem, IntDomain }
import cspom.CSPOMConstraint
import scala.collection.immutable.SortedSet
import cspom.CSPOMConstraint
import concrete.UNSAT
import concrete.UNSATObject
import Generator._
import concrete.constraint.semantic.Square

import Generator._

final object SquareGenerator extends Generator {

  override def genFunctional(constraint: CSPOMConstraint, result: C2Conc)(implicit problem: Problem) = {
    val Seq(v0: C21D) = constraint.arguments map cspom2concrete

    (result, v0) match {
      case (Const(result), Const(v0)) =>
        if (result == v0 * v0) {
          Some(Nil)
        } else {
          throw UNSATObject
        }
      case (Const(result), Var(v0)) =>
        restrictDomain(v0, Square.sqrt(result))
        Some(Nil)
      case (Var(result), Const(v0)) =>
        restrictDomain(result, Seq(v0 * v0))
        Some(Nil)
      case (Var(result), Var(v0)) =>
        if (v0.dom.undefined && result.dom.undefined) {
          None
        } else {
          if (!v0.dom.undefined) {
            restrictDomain(result, v0.dom.values.map(v => v * v))
          }
          if (!result.dom.undefined) {
            restrictDomain(v0, result.dom.values.flatMap(
              v => Square.sqrt(v).map(s => Seq(-s, s)).getOrElse(Nil)))
          }

          Some(Seq(new Square(result, v0)))

        }
      case _ => None

    }

  }

}
