package cspfj.generator.constraint;

import java.util.List;

import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;

import cspfj.constraint.semantic.Gcc;
import cspfj.constraint.semantic.Gcc.Bounds;
import cspfj.exception.FailedGenerationException;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspom.constraint.CSPOMConstraint;
import cspom.constraint.GeneralConstraint;

final class GccGenerator(problem: Problem) extends AbstractGenerator(problem) {
  def generate(constraint: CSPOMConstraint) = {
    require(constraint.isInstanceOf[GeneralConstraint])

    val scope = constraint.scope map getSolverVariable

    if (scope exists (_.getDomain == null)) {
      false
    } else {
      val params = constraint.parameters.split(", +");

      require(params.size % 3 == 0, "3 parameters per value");

      val bounds = params.grouped(3) map (p => new Bounds(p(0).toInt, p(1).toInt, p(2).toInt))

      addConstraint(new Gcc(scope, bounds));
      true;
    }

  }
}