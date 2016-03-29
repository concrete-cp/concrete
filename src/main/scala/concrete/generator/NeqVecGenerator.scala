package concrete.generator;

import Generator.cspom2concreteSeq
import concrete.constraint.semantic.NeqVec
import cspom.CSPOMConstraint

final object NeqVecGenerator extends Generator {

  override def gen(constraint: CSPOMConstraint[Boolean])(implicit variables: VarMap) = {
    val Seq(x, y) = constraint.arguments map cspom2concreteSeq

    Seq(new NeqVec(x.map(_.asVariable).toArray, y.map(_.asVariable).toArray))

  }

}
