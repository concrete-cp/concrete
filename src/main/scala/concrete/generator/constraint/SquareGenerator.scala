package concrete.generator.constraint;

import concrete.constraint.semantic.Abs
import concrete.{ Variable, Problem, IntDomain }
import cspom.CSPOMConstraint
import scala.collection.immutable.SortedSet
import cspom.CSPOMConstraint
import concrete.UNSAT
import concrete.UNSATObject
import AbstractGenerator._
import concrete.constraint.semantic.Square

final class SquareGenerator(problem: Problem) extends AbstractGenerator(problem) {

  override def genFunctional(constraint: CSPOMConstraint, result: C2Conc) = {
    val Seq(v0: C21D) = constraint.arguments map cspom2concrete

    (result, v0) match {
      case (C2C(result), C2C(v0)) =>
        result == v0 * v0 || (throw UNSATObject)
      case (C2C(result), C2V(v0)) =>
        restrictDomain(v0, Square.sqrt(result))
        true
      case (C2V(result), C2C(v0)) =>
        restrictDomain(result, Seq(v0 * v0))
        true
      case (C2V(result), C2V(v0)) =>
        if (v0.dom.undefined && result.dom.undefined) {
          false
        } else {
          if (!v0.dom.undefined) {
            restrictDomain(result, v0.dom.values.map(v => v * v))
          }
          if (!result.dom.undefined) {
            restrictDomain(v0, result.dom.values.flatMap(
              v => Square.sqrt(v).map(s => Seq(-s, s)).getOrElse(Nil)))
          }

          addConstraint(new Square(result, v0))
          true
        }
      case _ => false

    }

  }

}
