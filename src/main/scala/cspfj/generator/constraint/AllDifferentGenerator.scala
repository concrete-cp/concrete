package cspfj.generator.constraint;

import java.util.List;

import com.google.common.base.Preconditions;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;

import cspfj.constraint.semantic.AllDifferent;
import cspfj.exception.FailedGenerationException;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspom.constraint.CSPOMConstraint;
import cspom.constraint.GeneralConstraint;

final class AllDifferentGenerator(problem: Problem) extends AbstractGenerator(problem) {
  def generate(constraint: CSPOMConstraint) {
    require(constraint.isInstanceOf[GeneralConstraint]);

    val solverVariables = constraint.scope map getSolverVariable
    if (solverVariables exists { _.getDomain == null }) {
      false
    } else {
      addConstraint(new AllDifferent(solverVariables: _*));
      true;
    }
  }

}
