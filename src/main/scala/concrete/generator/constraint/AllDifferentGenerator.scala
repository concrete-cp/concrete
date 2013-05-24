package concrete.generator.constraint;

import concrete.{ Variable, Problem }
import cspom.constraint.{ GeneralConstraint, CSPOMConstraint }
import concrete.constraint.semantic.{ AllDifferent2C, AllDifferentBC }

final class AllDifferentGenerator(problem: Problem) extends AbstractGenerator(problem) {
  override def generateGeneral(constraint: GeneralConstraint) = {
    val solverVariables = constraint.scope map cspom2concrete
    if (solverVariables.exists(_.dom.undefined)) {
      false
    } else {
      addConstraint(new AllDifferent2C(solverVariables: _*));
      addConstraint(new AllDifferentBC(solverVariables: _*));
      true;
    }
  }

}
