package cspfj.generator.constraint;


import cspfj.constraint.semantic.LexLeq
import cspfj.problem.{Variable, Problem}
import cspom.constraint.CSPOMConstraint

final class LexLeqGenerator(problem: Problem) extends AbstractGenerator(problem) {
  def generate(constraint: CSPOMConstraint) = {
    val solverVariables = constraint.scope map getSolverVariable

    if (solverVariables exists (_.getDomain == null)) {
      false
    } else {
      addConstraint(new LexLeq(solverVariables: _*));
      true;
    }

  }
}
