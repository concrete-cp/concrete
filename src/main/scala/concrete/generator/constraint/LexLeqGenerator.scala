package concrete.generator.constraint;

import concrete.{ Variable, Problem }
import cspom.CSPOMConstraint
import concrete.constraint.semantic.{ AllDifferent2C, AllDifferentBC }
import concrete.constraint.semantic.Sum
import concrete.constraint.semantic.LexLeq

final class LexLeqGenerator(problem: Problem) extends AbstractGenerator(problem) {
  override def gen(constraint: CSPOMConstraint) = {
    val Seq(x, y) = constraint.arguments map cspom2concreteSeqVar map (_.toArray)
    require(x.length == y.length)

    if ((x.iterator ++ y) exists { _.dom.undefined }) {
      false
    } else {
      addConstraint(new LexLeq(x, y));
      true;
    }
  }

}
