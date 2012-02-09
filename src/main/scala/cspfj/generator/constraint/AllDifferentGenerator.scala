package cspfj.generator.constraint;

import cspfj.constraint.semantic.AllDifferent
import cspfj.problem.{Variable, Problem}
import cspom.constraint.{GeneralConstraint, CSPOMConstraint}
import cspfj.constraint.semantic.AllDifferentAC

final class AllDifferentGenerator(problem: Problem) extends AbstractGenerator(problem) {
  def generate(constraint: CSPOMConstraint) = {
    require(constraint.isInstanceOf[GeneralConstraint]);

    val solverVariables = constraint.scope map cspom2cspfj
    if (solverVariables exists { _.dom == null }) {
      false
    } else {
      addConstraint(new AllDifferentAC(solverVariables: _*));
      true;
    }
  }

}
