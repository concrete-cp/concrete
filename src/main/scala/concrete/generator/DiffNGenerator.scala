package concrete.generator

import Generator.cspom2concreteSeq
import cspom.CSPOMConstraint
import concrete.constraint.semantic.DiffN
import concrete.constraint.semantic.DiffNSpaceChecker

final class DiffNGenerator(pg: ProblemGenerator) extends Generator {

  override def gen(constraint: CSPOMConstraint[Boolean])(implicit variables: VarMap) = {
    val Seq(x, y, dx, dy) = constraint.arguments

    val xv = cspom2concreteSeq(x).map(_.asVariable(pg)).toArray
    val yv = cspom2concreteSeq(y).map(_.asVariable(pg)).toArray
    val dxv = cspom2concreteSeq(dx).map(_.asVariable(pg)).toArray
    val dyv = cspom2concreteSeq(dy).map(_.asVariable(pg)).toArray

    Seq(
      new DiffN(xv, yv, dxv, dyv),
      new DiffNSpaceChecker(xv, yv, dxv, dyv))

  }

}