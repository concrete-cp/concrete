package concrete.generator.constraint;

import concrete.constraint.Constraint
import concrete.constraint.semantic.Gt
import concrete.constraint.semantic.ReifiedConstraint
import concrete.generator.FailedGenerationException
import concrete.Problem
import concrete.Variable
import cspom.CSPOMConstraint
import cspom.variable.CSPOMExpression
import concrete.UNSATObject

final class GtGenerator(problem: Problem) extends AbstractGenerator(problem) {

  override def gen(constraint: CSPOMConstraint) = {
    require(constraint.arguments.size == 2,
      "Comparison constraints must have exactly two arguments");

    val solverVariables = constraint.arguments map cspom2concrete1D;

    if (solverVariables.collect { case C2V(v) => v } exists (_.dom.undefined)) {
      false
    } else {
      constraint.function match {
        case 'gt => gte(solverVariables(0), solverVariables(1), true);
        case 'ge => gte(solverVariables(0), solverVariables(1), false);
        case 'lt => gte(solverVariables(1), solverVariables(0), true);
        case 'le => gte(solverVariables(1), solverVariables(0), false);
        case _ => throw new FailedGenerationException("Unhandled constraint " + constraint);
      }
      true
    }

  }

  private def gte(v0: C21D, v1: C21D, strict: Boolean): Unit = (v0, v1) match {
    case (C2C(v0), C2C(v1)) => require(v0 > v1 || (!strict && v0 >= v1))
    case (C2V(v0), C2C(v1)) =>
      if (strict) {
        v0.dom.removeToVal(v1)
      } else {
        v0.dom.removeToVal(v1 - 1)
      }
    case (C2C(v0), C2V(v1)) =>
      if (strict) {
        v1.dom.removeFromVal(v0)
      } else {
        v1.dom.removeFromVal(v0 + 1)
      }
    case (C2V(v0), C2V(v1)) =>
      addConstraint(new Gt(v0, v1, strict))
  }

  override def genReified(constraint: CSPOMConstraint, result: Variable) = {
    require(constraint.arguments.size == 2,
      "Comparison constraints must have exactly two arguments");

    val arguments = constraint.arguments map cspom2concreteVar

    if (arguments exists (_.dom.undefined)) {
      false
    } else {

      AbstractGenerator.booleanDomain(result);

      addConstraint(constraint.function match {
        case 'gt =>
          new ReifiedConstraint(
            result,
            new Gt(arguments(0), arguments(1), true),
            new Gt(arguments(1), arguments(0), false));
        case 'ge =>
          new ReifiedConstraint(
            result,
            new Gt(arguments(0), arguments(1), false),
            new Gt(arguments(1), arguments(0), true));
        case 'lt =>
          new ReifiedConstraint(
            result,
            new Gt(arguments(1), arguments(0), true),
            new Gt(arguments(0), arguments(1), false));
        case 'le =>
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
