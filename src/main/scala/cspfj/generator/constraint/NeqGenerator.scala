package cspfj.generator.constraint;

import java.util.List;

import com.google.common.base.Preconditions;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;

import cspfj.constraint.Constraint;
import cspfj.constraint.semantic.Eq;
import cspfj.constraint.semantic.Neq;
import cspfj.constraint.semantic.ReifiedConstraint;
import cspfj.exception.FailedGenerationException;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspom.constraint.CSPOMConstraint;
import cspom.constraint.FunctionalConstraint;
import cspom.constraint.GeneralConstraint;

final class NeqGenerator(problem: Problem) extends AbstractGenerator(problem) {

  private def generateGeneral(constraint: GeneralConstraint) = {
    require(constraint.arity == 2,
      "Comparison constraints must have exactly two arguments");

    val scope = constraint.scope map getSolverVariable

    if (scope exists { _.getDomain == null }) {
      null
    } else {
      new Neq(scope(0), scope(1))
    }
  }

  private def generateReified(constraint: FunctionalConstraint) = {
    require(constraint.arguments.size == 2,
      "Comparison constraints must have exactly two arguments");

    val arguments = constraint.arguments map getSolverVariable

    if (arguments exists { _.getDomain == null }) {
      null
    } else {

      val result = getSolverVariable(constraint.result);
      booleanDomain(result);
      new ReifiedConstraint(result, new Neq(arguments(0),
        arguments(1)), new Eq(arguments(0), arguments(1)));
    }
  }

  @throws(classOf[FailedGenerationException])
  def generate(constraint: CSPOMConstraint) = {
    val generated = constraint match {
      case gC: GeneralConstraint => generateGeneral(gC)
      case fC: FunctionalConstraint => generateReified(fC)
      case _ =>
        throw new FailedGenerationException(constraint
          + " is not supported");
    }

    if (generated == null) {
      false;
    } else {
      addConstraint(generated);
      true;
    }
  }

}
