package concrete.generator

;

import concrete.constraint.semantic.BinPacking
import concrete.generator.Generator.cspom2concrete
import cspom.CSPOMConstraint

final class BinPackingGenerator(pg: ProblemGenerator) extends Generator {
  override def gen(constraint: CSPOMConstraint[Boolean])(implicit variables: VarMap) = {

    val Seq(l, b, w) = constraint.arguments.map(cspom2concrete)

    @unchecked
    val Sequence(load, indices) = l
    @unchecked
    val Sequence(bin, _) = b
    @unchecked
    val Sequence(weights, _) = w

    val wi = weights.map {
      case Const(i: Int) => i
      case _ => throw new IllegalStateException()
    }
    require(bin.length == wi.length)

    Seq(BinPacking(load.map(_.asVariable(pg)), indices.head, bin.map(_.asVariable(pg)), wi))
  }

}
