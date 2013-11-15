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

final class NeqGenerator(problem: Problem) extends AbstractGenerator(problem) {

  override def gen(constraint: CSPOMConstraint) = {

    val Seq(v0, v1) = constraint.arguments map cspom2concrete1D

    if (v0.undefined || v1.undefined) {
      false
    } else {
      (v0, v1) match {
        case (Const(v0), Const(v1)) => (v0 != v1) || (throw UNSATObject)
        case (Const(v0), Var(v1)) => v1.dom.removeVal(v0)
        case (Var(v0), Const(v1)) => v0.dom.removeVal(v1)
        case (Var(v0), Var(v1)) => addConstraint(new Neq(v0, v1))

      }

      true
    }
  }

  override def genReified(constraint: CSPOMConstraint, result: Variable) = {
    require(constraint.arguments.size == 2,
      "Comparison constraints must have exactly two arguments");

    val Seq(v0, v1) = constraint.arguments map cspom2concrete1D

    if (Seq(v0, v1) collect { case Var(v) => v } exists (_.dom.undefined)) {
      false
    } else {
      AbstractGenerator.booleanDomain(result);
      (v0, v1) match {
        case (Const(v0), Const(v1)) =>
          if (v0 == v1) {
            result.dom.setSingle(1)
          } else {
            result.dom.setSingle(0)
          }
        case (Var(v0), Const(v1)) =>
          addConstraint(new ReifiedNeq(result, v0, v1))
        case (Const(v0), Var(v1)) =>
          addConstraint(new ReifiedNeq(result, v1, v0))
        case (Var(v0), Var(v1)) =>
          addConstraint(new ReifiedConstraint(
            result,
            new Neq(v0, v1),
            new Eq(v0, v1)))
      }

      true
    }
  }

}
