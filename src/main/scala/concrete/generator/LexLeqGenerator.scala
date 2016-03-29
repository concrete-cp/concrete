package concrete.generator;

import Generator.cspom2concreteSeq
import concrete.constraint.semantic.LexLeq
import cspom.CSPOMConstraint

object LexLeqGenerator extends Generator {
  override def gen(constraint: CSPOMConstraint[Boolean])(implicit variables: VarMap) = {
    val Seq(x, y) = constraint.arguments map cspom2concreteSeq map (_.toArray)
    require(x.length == y.length)

    Seq(new LexLeq(x.map(_.asVariable), y.map(_.asVariable)))
  }

}
