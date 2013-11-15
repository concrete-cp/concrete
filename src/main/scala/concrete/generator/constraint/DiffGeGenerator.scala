package concrete.generator.constraint;

import concrete.constraint.semantic.{ ReifiedConstraint, Gt }
import concrete.{ Variable, Problem }
import cspom.CSPOMConstraint
import cspom.variable.CSPOMTrue
import cspom.variable.CSPOMBool
import cspom.variable.BoolVariable
import concrete.UNSATObject
import concrete.BooleanDomain
import cspom.variable.CSPOMFalse

/**
 * Generator for "diffGe" constraints defined as x - y >= k (k must be a constant)
 */
final class DiffGeGenerator(problem: Problem) extends AbstractGenerator(problem) {

  override def gen(constraint: CSPOMConstraint): Boolean = {

    val Seq(x: C21D, y: C21D, Const(k)) = constraint.arguments map cspom2concrete

    (x, y) match {
      case (Const(x), Const(y)) => ((x - y) >= k) || (throw UNSATObject)
      case (Var(x), Const(y)) =>
        if (x.dom.undefined) {
          false
        } else {
          x.dom.filterValues(_ - y >= k)
          true
        }
      case (Const(x), Var(y)) =>
        if (y.dom.undefined) {
          false
        } else {
          y.dom.filterValues(x - _ >= k)
          true
        }
      case (Var(x), Var(y)) =>
        if (x.dom.undefined || y.dom.undefined) {
          false
        } else {
          addConstraint(new Gt(x, -k, y, false))
          true
        }
    }

  }

  override def genReified(constraint: CSPOMConstraint, result: Variable): Boolean = {

    val rd = AbstractGenerator.booleanDomain(result)
    val Seq(x: C21D, y: C21D, Const(k)) = constraint.arguments map cspom2concrete

    (x, y) match {
      case (Const(x), Const(y)) =>
        if (x - y >= k) { rd.setTrue() } else { rd.setFalse() }
        true
      case (Var(x), Const(y)) => ???
      case (Const(x), Var(y)) => ???
      case (Var(x), Var(y)) =>
        addConstraint(new ReifiedConstraint(
          result,
          new Gt(x, -k, y, false),
          new Gt(y, k, x, true)))
        true
    }

  }

}
