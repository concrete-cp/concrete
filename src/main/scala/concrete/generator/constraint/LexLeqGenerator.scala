package concrete.generator.constraint;

import concrete.{ Variable, Problem }
import cspom.CSPOMConstraint
import concrete.constraint.semantic.{ AllDifferent2C, AllDifferentBC }
import concrete.constraint.semantic.Sum
import concrete.constraint.semantic.LexLeq
import Generator._

final object LexLeqGenerator extends Generator {
  override def gen(constraint: CSPOMConstraint)(implicit problem: Problem) = {
    val Seq(x, y) = constraint.arguments map cspom2concreteSeqVar map (_.toArray)
    require(x.length == y.length)

    if ((x.iterator ++ y) exists { _.dom.undefined }) {
      None
    } else {
      Some(Seq(new LexLeq(x, y)));
    }
  }

}
