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

  override def genFunctional(constraint: CSPOMConstraint[_], result: C2Conc)(implicit variables: VarMap) = {
    val Seq(v0: C21D) = constraint.arguments map cspom2concrete

    (result, v0) match {
      case (Const(result), Const(v0)) =>
        if (result == v0 * v0) {
          Nil
        } else {
          throw UNSATObject
        }
      case (Const(result), Var(v0)) =>
        //restrictDomain(v0, Square.sqrt(result).map(s => Iterator(-s, s)).getOrElse(Iterator.empty))
        require(Square.sqrt(result).exists(v0.dom.presentVal))
        Nil
      case (Var(result), Const(v0)) =>
        //restrictDomain(result, Iterator(v0 * v0))
        require(result.dom.presentVal(v0 * v0))
        Nil
      case (Var(result), Var(v0)) =>
        //        if (v0.dom.undefined && result.dom.undefined) {
        //          None
        //        } else {
        //          if (!v0.dom.undefined) {
        //            restrictDomain(result, v0.dom.values.map(v => v * v))
        //          }
        //          if (!result.dom.undefined) {
        //            restrictDomain(v0, result.dom.values.flatMap(
        //              v => Square.sqrt(v).toIterable.flatMap(s => Seq(-s, s))))
        //          }

        Seq(new Square(result, v0))
    }

  }

}
