package concrete.generator.constraint;

import concrete.{ Variable, Problem }
import cspom.CSPOMConstraint
import concrete.constraint.semantic.{ AllDifferent2C, AllDifferentBC }
import concrete.constraint.semantic.Sum
import concrete.constraint.semantic.LexLeq
import Generator._

final object LexLeqGenerator extends Generator {
  override def gen(constraint: CSPOMConstraint[Boolean])(implicit variables: VarMap) = {
    val Seq(x, y) = constraint.arguments map cspom2concreteSeq map (_.toArray)
    require(x.length == y.length)
    

    Seq(new LexLeq(x.map(_.asVariable), y.map(_.asVariable)))
  }

}
