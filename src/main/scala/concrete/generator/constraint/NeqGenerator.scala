package concrete.generator.constraint;

import concrete.constraint.Constraint;
import concrete.constraint.semantic.Eq;
import concrete.constraint.semantic.Neq;
import concrete.constraint.semantic.ReifiedConstraint;
import concrete.generator.FailedGenerationException;
import concrete.Problem;
import concrete.Variable;
import cspom.CSPOMConstraint;

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

    val arguments = constraint.arguments map cspom2concreteVar

    if (arguments exists { _.dom.undefined }) {
      false
    } else {

      AbstractGenerator.booleanDomain(result);
      addConstraint(new ReifiedConstraint(
        result,
        new Neq(arguments(0), arguments(1)),
        new Eq(arguments(0), arguments(1))))
      true
    }
  }

}
