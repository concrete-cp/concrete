package cspfj.generator.constraint;

import cspfj.{ Variable, Problem }
import cspom.constraint.{ GeneralConstraint, CSPOMConstraint }
import cspfj.constraint.semantic.{ AllDifferent2C, AllDifferentBC }
import cspfj.constraint.semantic.ZeroSum
import cspfj.constraint.semantic.LexLeq

final class LexLeqGenerator(problem: Problem) extends AbstractGenerator(problem) {
  override def generateGeneral(constraint: GeneralConstraint) = {
    val solverVariables = constraint.scope map cspom2cspfj toArray

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
