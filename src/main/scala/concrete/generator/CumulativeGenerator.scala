package concrete.generator

import Generator.cspom2concrete1D
import Generator.cspom2concreteSeq
import concrete.constraint.semantic.Cumulative
import cspom.CSPOMConstraint
import concrete.constraint.semantic.CumulativeEnergy

final class CumulativeGenerator(pg: ProblemGenerator) extends Generator {

  override def gen(constraint: CSPOMConstraint[Boolean])(implicit variables: VarMap) = {
    val Seq(s, d, r, b) = constraint.arguments

    val svars = cspom2concreteSeq(s).map(_.asVariable(pg)).toArray
    val dvars = cspom2concreteSeq(d).map(_.asVariable(pg)).toArray
    val rvars = cspom2concreteSeq(r).map(_.asVariable(pg)).toArray
    val bvar = cspom2concrete1D(b).asVariable(pg)

    Seq(
      new Cumulative(svars, dvars, rvars, bvar),
      new CumulativeEnergy(svars, dvars, rvars, bvar))//.take(1)

  }

}