package concrete.generator

import Generator.cspom2concreteSeq
import cspom.CSPOMConstraint

import concrete.constraint.semantic.ClauseConstraint
import cspom.variable.CSPOMSeq

object ClauseGenerator extends Generator {

  override def gen(constraint: CSPOMConstraint[Boolean])(implicit variables: VarMap) = {
    val Seq(pos: CSPOMSeq[_], neg: CSPOMSeq[_]) = constraint.arguments
    if (pos.exists(_.isTrue) || neg.exists(_.isFalse)) {
      Seq()
    } else {
      val posConc = pos.map { v => Generator.cspom2concreteVar(v)(variables) }.toArray
      val negConc = neg.map { v => Generator.cspom2concreteVar(v)(variables) }.toArray
      Seq(new ClauseConstraint(posConc, negConc))
    }

  }

}