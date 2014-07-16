package concrete.generator.constraint;

import concrete.constraint.Constraint
import concrete.constraint.semantic.Neq
import concrete.constraint.semantic.ReifiedConstraint
import concrete.generator.FailedGenerationException
import concrete.Problem
import concrete.Variable
import cspom.CSPOMConstraint
import concrete.constraint.semantic.ReifiedNeq
import concrete.UNSATObject
import Generator._
import concrete.constraint.semantic.EqAC

final object NeqGenerator extends Generator {

  override def gen(constraint: CSPOMConstraint[Boolean])(implicit variables: VarMap) = {

    val Seq(v0, v1) = constraint.arguments map cspom2concrete1D

    (v0, v1) match {
      case (Const(v0), Const(v1)) =>
        if (v0 != v1) Nil else throw UNSATObject
      case (Const(v0), Var(v1)) =>
        v1.dom.removeVal(v0)
        Nil
      case (Var(v0), Const(v1)) =>
        v0.dom.removeVal(v1)
        Nil
      case (Var(v0), Var(v1)) => Seq(new Neq(v0, v1))

    }

  }

  override def genFunctional(constraint: CSPOMConstraint[_], r: C2Conc)(implicit variables: VarMap) = {
    val Var(result) = r
    require(constraint.arguments.size == 2,
      "Comparison constraints must have exactly two arguments");

    val Seq(v0, v1) = constraint.arguments map cspom2concrete1D

    (v0, v1) match {
      case (Const(v0), Const(v1)) =>
        if (v0 == v1) {
          result.dom.setSingle(1)
        } else {
          result.dom.setSingle(0)
        }
        Nil
      case (Var(v0), Const(v1)) =>
        Seq(new ReifiedNeq(result, v0, v1))
      case (Const(v0), Var(v1)) =>
        Seq(new ReifiedNeq(result, v1, v0))
      case (Var(v0), Var(v1)) =>
        Seq(new ReifiedConstraint(
          result,
          new Neq(v0, v1),
          new EqAC(v0, v1)))
    }

  }

}
