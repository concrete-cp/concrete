package concrete.generator.constraint;

import Generator.cspom2concreteVar
import concrete.constraint.semantic.SquareAC
import concrete.constraint.semantic.SquareBC
import cspom.CSPOMConstraint

final object SquareGenerator extends Generator {

  override def genFunctional(constraint: CSPOMConstraint[_], result: C2Conc)(implicit variables: VarMap) = {
    val Seq(v0) = constraint.arguments map cspom2concreteVar
    val Var(r) = result
    Seq(new SquareBC(r, v0), new SquareAC(r, v0))
//
//    (result, v0) match {
//      case (Const(result), Const(v0)) =>
//        if (result == v0 * v0) {
//          Nil
//        } else {
//          throw UNSATObject
//        }
//      case (Const(result), Var(v0)) =>
//        //restrictDomain(v0, Square.sqrt(result).map(s => Iterator(-s, s)).getOrElse(Iterator.empty))
//        require(Square.sqrt(result).exists(v0.dom.presentVal))
//        Nil
//      case (Var(result), Const(v0)) =>
//        //restrictDomain(result, Iterator(v0 * v0))
//        require(result.dom.presentVal(v0 * v0))
//        Nil
//      case (Var(result), Var(v0)) =>
//      //        if (v0.dom.undefined && result.dom.undefined) {
//      //          None
//      //        } else {
//      //          if (!v0.dom.undefined) {
//      //            restrictDomain(result, v0.dom.values.map(v => v * v))
//      //          }
//      //          if (!result.dom.undefined) {
//      //            restrictDomain(v0, result.dom.values.flatMap(
//      //              v => Square.sqrt(v).toIterable.flatMap(s => Seq(-s, s))))
//      //          }
//
//      case _                      => throw new AssertionError("No sequences allowed")
//    }

  }

}
