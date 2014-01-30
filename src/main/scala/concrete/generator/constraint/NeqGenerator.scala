package concrete.generator.constraint;

import concrete.constraint.Constraint
import concrete.constraint.semantic.Eq
import concrete.constraint.semantic.Neq
import concrete.constraint.semantic.ReifiedConstraint
import concrete.generator.FailedGenerationException
import concrete.Problem
import concrete.Variable
import cspom.CSPOMConstraint
import concrete.constraint.semantic.ReifiedNeq
import concrete.UNSATObject

import Generator._

final object NeqGenerator extends Generator {

  override def gen(constraint: CSPOMConstraint)(implicit variables: VarMap) = {

    val Seq(v0, v1) = constraint.arguments map cspom2concrete1D

    if (undefinedVar(v0, v1).nonEmpty) {
      None
    } else {
      (v0, v1) match {
        case (Const(v0), Const(v1)) =>
          if (v0 != v1) Some(Nil) else throw UNSATObject
        case (Const(v0), Var(v1)) =>
          v1.dom.removeVal(v0)
          Some(Nil)
        case (Var(v0), Const(v1)) =>
          v0.dom.removeVal(v1)
          Some(Nil)
        case (Var(v0), Var(v1)) => Some(Seq(new Neq(v0, v1)))

      }

    }
  }

  override def genFunctional(constraint: CSPOMConstraint, r: C2Conc)(implicit variables: VarMap) = {
    val Var(result) = r
    require(constraint.arguments.size == 2,
      "Comparison constraints must have exactly two arguments");

    val Seq(v0, v1) = constraint.arguments map cspom2concrete1D

    if (undefinedVar(v0, v1).nonEmpty) {
      None
    } else {
      Generator.booleanDomain(result);
      (v0, v1) match {
        case (Const(v0), Const(v1)) =>
          if (v0 == v1) {
            result.dom.setSingle(1)
          } else {
            result.dom.setSingle(0)
          }
          Some(Nil)
        case (Var(v0), Const(v1)) =>
          Some(Seq(new ReifiedNeq(result, v0, v1)))
        case (Const(v0), Var(v1)) =>
          Some(Seq(new ReifiedNeq(result, v1, v0)))
        case (Var(v0), Var(v1)) =>
          Some(Seq(new ReifiedConstraint(
            result,
            new Neq(v0, v1),
            new Eq(v0, v1))))
      }

    }
  }

}
