package concrete.generator.constraint;

import concrete.constraint.Constraint
import concrete.constraint.semantic.Eq
import concrete.constraint.semantic.Neq
import concrete.constraint.semantic.ReifiedConstraint
import concrete.generator.FailedGenerationException
import concrete.Problem
import concrete.Variable
import cspom.CSPOMConstraint;
import concrete.constraint.semantic.ReifiedNeq

final class NeqGenerator(problem: Problem) extends AbstractGenerator(problem) {

  override def gen(constraint: CSPOMConstraint) = {
    require(constraint.arity == 2,
      "Comparison constraints must have exactly two arguments");

    val scope = constraint.arguments map cspom2concreteVar

    if (scope exists { _.dom.undefined }) {
      false
    } else {
      addConstraint(new Neq(scope(0), scope(1)))
      true
    }
  }

  override def genReified(constraint: CSPOMConstraint, result: Variable) = {
    require(constraint.arguments.size == 2,
      "Comparison constraints must have exactly two arguments");

    val Seq(v0, v1) = constraint.arguments map cspom2concrete1D

    if (Seq(v0, v1) collect { case C2V(v) => v } exists (_.dom.undefined)) {
      false
    } else {
      AbstractGenerator.booleanDomain(result);
      (v0, v1) match {
        case (C2C(v0), C2C(v1)) =>
          if (v0 == v1) {
            result.dom.setSingle(1)
          } else {
            result.dom.setSingle(0)
          }
        case (C2V(v0), C2C(v1)) =>
          addConstraint(new ReifiedNeq(result, v0, v1))
        case (C2C(v0), C2V(v1)) =>
          addConstraint(new ReifiedNeq(result, v1, v0))
        case (C2V(v0), C2V(v1)) =>
          addConstraint(new ReifiedConstraint(
            result,
            new Neq(v0, v1),
            new Eq(v0, v1)))
      }

      true
    }
  }

}
