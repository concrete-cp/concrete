package concrete.generator.constraint;

import concrete.constraint.semantic.{ ReifiedConstraint, Gt }
import concrete.{ Variable, Problem }
import cspom.CSPOMConstraint
import cspom.variable.CSPOMTrue
import cspom.variable.BoolVariable
import concrete.UNSATObject
import concrete.BooleanDomain
import cspom.variable.CSPOMFalse
import Generator._

/**
 * Generator for "diffGe" constraints defined as x - y >= k (k must be a constant)
 */
final object DiffGeGenerator extends Generator {

  override def gen(constraint: CSPOMConstraint[Boolean])(implicit variables: VarMap) = {

    val Seq(x: C21D, y: C21D, Const(k)) = constraint.arguments map cspom2concrete

    if (undefinedVar(x, y).nonEmpty) {
      None
    } else {

      (x, y) match {
        case (Const(x), Const(y)) => if ((x - y) >= k) {
          Some(Nil)
        } else { throw UNSATObject }

        case (Var(x), Const(y)) =>

          x.dom.filterValues(_ - y >= k)
          Some(Nil)
        case (Const(x), Var(y)) =>

          y.dom.filterValues(x - _ >= k)
          Some(Nil)
        case (Var(x), Var(y)) =>

          Some(Seq(new Gt(x, -k, y, false)))

      }
    }

  }

  override def genFunctional(constraint: CSPOMConstraint[_], r: C2Conc)(
      implicit variables: VarMap) = {
    val Var(result) = r
    val rd = Generator.booleanDomain(result)
    val Seq(x: C21D, y: C21D, Const(k)) = constraint.arguments map cspom2concrete

    (x, y) match {
      case (Const(x), Const(y)) =>
        if (x - y >= k) { rd.setTrue() } else { rd.setFalse() }
        Some(Nil)
      case (Var(x), Const(y)) => ???
      case (Const(x), Var(y)) => ???
      case (Var(x), Var(y)) =>
        Some(Seq(new ReifiedConstraint(
          result,
          new Gt(x, -k, y, false),
          new Gt(y, k, x, true))))
    }

  }

}
