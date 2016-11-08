package concrete.generator;

import Generator.cspom2concrete
import cspom.CSPOMConstraint
import concrete.constraint.semantic.BinPacking

final class BinPackingGenerator(pg: ProblemGenerator) extends Generator {
  override def gen(constraint: CSPOMConstraint[Boolean])(implicit variables: VarMap) = {

    val Seq(l, b, w) = constraint.arguments.map(cspom2concrete)

    val Sequence(load, indices) = l
    val Sequence(bin, _) = b
    val Sequence(weights, _) = w
    val wi = weights.map { case Const(i: Int) => i }
    require(bin.length == wi.length)

    Seq(BinPacking(load.map(_.asVariable(pg)), indices.head, bin.map(_.asVariable(pg)), wi))
  }

}
