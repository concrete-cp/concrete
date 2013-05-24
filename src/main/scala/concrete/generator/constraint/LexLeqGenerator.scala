package concrete.generator.constraint;

import concrete.{ Variable, Problem }
import cspom.constraint.{ GeneralConstraint, CSPOMConstraint }
import concrete.constraint.semantic.{ AllDifferent2C, AllDifferentBC }
import concrete.constraint.semantic.ZeroSum
import concrete.constraint.semantic.LexLeq

final class LexLeqGenerator(problem: Problem) extends AbstractGenerator(problem) {
  override def generateGeneral(constraint: GeneralConstraint) = {
    val solverVariables = constraint.scope map cspom2concrete toArray

    if (solverVariables exists { _.dom.undefined }) {
      false
    } else {
      val (s1, s2) = solverVariables.splitAt(solverVariables.length / 2)
      require(s1.length == s2.length)
      val c = new LexLeq(s1, s2)
      //println(c)
      addConstraint(c);
      true;
    }
  }

}
