package cspfj.generator.constraint;

import cspfj.{ Variable, Problem }
import cspom.constraint.{ GeneralConstraint, CSPOMConstraint }
import cspfj.constraint.semantic.{ AllDifferent2C, AllDifferentBC }

final class AllDifferentGenerator(problem: Problem) extends AbstractGenerator(problem) {
  override def generateGeneral(constraint: GeneralConstraint) = {
    val solverVariables = constraint.scope map cspom2cspfj
    if (solverVariables exists { _.dom == null }) {
      false
    } else {
      addConstraint(new AllDifferent2C(solverVariables: _*));
      addConstraint(new AllDifferentBC(solverVariables: _*));
      true;
    }
  }

}
