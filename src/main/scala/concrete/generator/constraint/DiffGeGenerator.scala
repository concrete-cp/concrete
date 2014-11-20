package concrete.generator.constraint;

import concrete.constraint.semantic.{ ReifiedConstraint, Gt }
import concrete.{ Variable, Problem }
import cspom.CSPOMConstraint
import cspom.variable.CSPOMConstant
import cspom.variable.BoolVariable
import concrete.UNSATObject
import concrete.BooleanDomain
import cspom.variable.CSPOMConstant
import Generator._

/**
 * Generator for "diffGe" constraints defined as x - y >= k (k must be a constant)
 */
final object DiffGeGenerator extends Generator {

  override def gen(constraint: CSPOMConstraint[Boolean])(implicit variables: VarMap) = {

    val Seq(Var(x), Var(y), Const(k)) = constraint.arguments map cspom2concrete
    //
    //    
    //    (x, y) match {
    //      case (Const(x), Const(y)) => if ((x - y) >= k) {
    //        Nil
    //      } else { throw UNSATObject }
    //
    //      case (Var(x), Const(y)) =>
    //        x.dom.filterValues(_ - y >= k)
    //        Nil
    //      case (Const(x), Var(y)) =>
    //
    //        y.dom.filterValues(x - _ >= k)
    //        Nil
    //      case (Var(x), Var(y)) =>

    Seq(new Gt(x, -k, y, false))

    //}

  }

  override def genFunctional(constraint: CSPOMConstraint[_], r: C2Conc)(
    implicit variables: VarMap) = {
    val Var(result) = r

    //val rd = result.dom.asInstanceOf[BooleanDomain] //Generator.booleanDomain(result)

    val Seq(Var(x), Var(y), Const(k)) = constraint.arguments map cspom2concrete

    //    (x, y) match {
    //      case (Const(x), Const(y)) =>
    //        if (x - y >= k) { rd.setTrue() } else { rd.setFalse() }
    //        Nil
    //      case (Var(x), Const(y)) => ???
    //      case (Const(x), Var(y)) => ???
    //      case (Var(x), Var(y)) =>
    Seq(new ReifiedConstraint(
      result,
      new Gt(x, -k, y, false),
      new Gt(y, k, x, true)))
    //  }

  }

}
