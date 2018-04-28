package concrete.generator

import Generator.cspom2concreteSeq
import concrete.Variable
import cspom.CSPOMConstraint
import concrete.constraint.semantic.{DiffN, DiffNSpaceChecker}

final class DiffNGenerator(pg: ProblemGenerator) extends Generator {

  override def gen(constraint: CSPOMConstraint[Boolean])(implicit variables: VarMap) = {
    constraint.arguments match {
      case Seq(x, y, dx, dy) =>
        val xv = cspom2concreteSeq(x).map(_.asVariable(pg)).toArray
        val yv = cspom2concreteSeq(y).map(_.asVariable(pg)).toArray
        val dxv = cspom2concreteSeq(dx).map(_.asVariable(pg)).toArray
        val dyv = cspom2concreteSeq(dy).map(_.asVariable(pg)).toArray

        diffn(xv, yv, dxv, dyv)


      case Seq(x, dx) =>
        val xs = cspom2concreteSeq(x)
          .map {
            case Sequence(s, _) => s.map(_.asVariable(pg)).toArray
          }
          .toArray
          .transpose
        val dxs = cspom2concreteSeq(dx)
          .map {
            case Sequence(s, _) => s.map(_.asVariable(pg)).toArray
          }
          .toArray
          .transpose

        require(xs.length == 2)
        require(dxs.length == 2)
        diffn(xs(0), xs(1), dxs(0), dxs(1))


    }


  }

  private def diffn(x: Array[Variable], y: Array[Variable], dx: Array[Variable], dy: Array[Variable]) = {
    Seq(
      new DiffN(x, y, dx, dy),
      new DiffNSpaceChecker(x, y, dx, dy))
  }

}