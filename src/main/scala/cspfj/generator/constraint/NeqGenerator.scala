package cspfj.generator.constraint;

import cspfj.constraint.Constraint;
import cspfj.constraint.semantic.Eq;
import cspfj.constraint.semantic.Neq;
import cspfj.constraint.semantic.ReifiedConstraint;
import cspfj.generator.FailedGenerationException;
import cspfj.Problem;
import cspfj.Variable;
import cspom.constraint.CSPOMConstraint;
import cspom.constraint.FunctionalConstraint;
import cspom.constraint.GeneralConstraint;

final class NeqGenerator(problem: Problem) extends AbstractGenerator(problem) {

  override def generateGeneral(constraint: GeneralConstraint) = {
    require(constraint.arity == 2,
      "Comparison constraints must have exactly two arguments");

    val scope = constraint.scope map cspom2cspfj

    if (scope exists { _.dom == null }) {
      false
    } else {
      addConstraint(new Neq(scope(0), scope(1)))
      true
    }
  }

  override def generateFunctional(constraint: FunctionalConstraint) = {
    require(constraint.arguments.size == 2,
      "Comparison constraints must have exactly two arguments");

    val arguments = constraint.arguments map cspom2cspfj

    if (arguments exists { _.dom == null }) {
      false
    } else {

      val result = cspom2cspfj(constraint.result);
      AbstractGenerator.booleanDomain(result);
      addConstraint(new ReifiedConstraint(
        result,
        new Neq (arguments(0), arguments(1)),
        new Eq(arguments(0), arguments(1))))
      true
    }
  }

}
