package cspfj.generator.constraint;

import cspfj.{ Variable, Problem }
import cspom.constraint.{ GeneralConstraint, CSPOMConstraint }
import cspfj.constraint.semantic.{ AllDifferent2C, AllDifferentBC }
import cspfj.constraint.semantic.ZeroSum

final class ZeroSumGenerator(problem: Problem) extends AbstractGenerator(problem) {
  override def generateGeneral(constraint: GeneralConstraint) = {
    val solverVariables = constraint.scope map cspom2cspfj toArray

    if (solverVariables exists { _.dom == null }) {
      false
    } else {
      addConstraint(new ZeroSum(solverVariables.map(v => 1), solverVariables));
      true;
    }
  }

}
