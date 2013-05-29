package concrete.generator.constraint;

import concrete.constraint.Constraint;
import concrete.constraint.semantic.Gt;
import concrete.constraint.semantic.ReifiedConstraint;
import concrete.generator.FailedGenerationException;
import concrete.Problem;
import concrete.Variable;
import cspom.constraint.CSPOMConstraint;
import cspom.constraint.FunctionalConstraint;
import cspom.constraint.GeneralConstraint;

final class GtGenerator(problem: Problem) extends AbstractGenerator(problem) {

  override def generateGeneral(constraint: GeneralConstraint) = {
    require(constraint.scope.size == 2,
      "Comparison constraints must have exactly two arguments");

    val solverVariables = constraint.scope map cspom2concrete;

    if (solverVariables.exists(_.dom.undefined)) {
      false
    } else {
      addConstraint(constraint.description match {
        case "gt" | ">" => new Gt(solverVariables(0), solverVariables(1), true);
        case "ge" | ">=" => new Gt(solverVariables(0), solverVariables(1), false);
        case "lt" | "<" => new Gt(solverVariables(1), solverVariables(0), true);
        case "le" | "<=" => new Gt(solverVariables(1), solverVariables(0), false);
        case _ => throw new FailedGenerationException("Unhandled constraint " + constraint);
      })
      true
    }

  }

  override def generateFunctional(constraint: FunctionalConstraint) = {
    require(constraint.arguments.size == 2,
      "Comparison constraints must have exactly two arguments");

    val arguments = constraint.arguments map cspom2concrete

    if (arguments exists (_.dom.undefined)) {
      false
    } else {

      val result = cspom2concrete(constraint.result);
      AbstractGenerator.booleanDomain(result);

      addConstraint(constraint.description match {
        case "gt" =>
          new ReifiedConstraint(
            result,
            new Gt(arguments(0), arguments(1), true),
            new Gt(arguments(1), arguments(0), false));
        case "ge" =>
          new ReifiedConstraint(
            result,
            new Gt(arguments(0), arguments(1), false),
            new Gt(arguments(1), arguments(0), true));
        case "lt" =>
          new ReifiedConstraint(
            result,
            new Gt(arguments(1), arguments(0), true),
            new Gt(arguments(0), arguments(1), false));
        case "le" =>
          new ReifiedConstraint(
            result,
            new Gt(arguments(1), arguments(0), false),
            new Gt(arguments(0), arguments(1), true));
        case _ =>
          throw new FailedGenerationException("Unhandled constraint " + constraint);
      })
      true

    }
  }

}
