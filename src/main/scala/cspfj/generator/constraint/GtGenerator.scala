package cspfj.generator.constraint;

import cspfj.constraint.Constraint;
import cspfj.constraint.semantic.Gt;
import cspfj.constraint.semantic.ReifiedConstraint;
import cspfj.generator.FailedGenerationException;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspom.constraint.CSPOMConstraint;
import cspom.constraint.FunctionalConstraint;
import cspom.constraint.GeneralConstraint;

final class GtGenerator(problem: Problem) extends AbstractGenerator(problem) {

  private def generateGeneral(constraint: GeneralConstraint) = {
    require(constraint.scope.size == 2,
      "Comparison constraints must have exactly two arguments");
    
    val solverVariables = constraint.scope map cspom2cspfj;

    if (solverVariables.exists(_.dom == null)) {
      null
    } else {
      constraint.description match {
        case "gt" => new Gt(solverVariables(0), solverVariables(1), true);
        case "ge" => new Gt(solverVariables(0), solverVariables(1), false);
        case "lt" => new Gt(solverVariables(1), solverVariables(0), true);
        case "le" => new Gt(solverVariables(1), solverVariables(0), false);
        case _ =>
          throw new FailedGenerationException("Unhandled constraint "
            + constraint);
      }
    }

  }

  def generateReified(constraint: FunctionalConstraint) = {
    require(constraint.arguments.size == 2,
      "Comparison constraints must have exactly two arguments");

    val arguments = constraint.arguments map cspom2cspfj

    if (arguments exists (_.dom == null)) {
      null
    } else {

      val result = cspom2cspfj(constraint.result);
      AbstractGenerator.booleanDomain(result);

      constraint.description match {
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
      }

    }
  }

  def generate(constraint: CSPOMConstraint) = {
    val generated = constraint match {
      case gC: GeneralConstraint => generateGeneral(gC);
      case fC: FunctionalConstraint => generateReified(fC);
      case _ => throw new IllegalArgumentException(constraint + " not supported");
    }

    if (generated == null) {
      false;
    } else {
      addConstraint(generated);
      true;
    }
  }
}
