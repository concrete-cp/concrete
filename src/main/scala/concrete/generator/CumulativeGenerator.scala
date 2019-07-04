package concrete.generator

import com.typesafe.scalalogging.LazyLogging
import concrete.constraint.semantic.{Cumulative, CumulativeEnergy}
import concrete.generator.Generator.{cspom2concrete1D, cspom2concreteSeq}
import cspom.CSPOMConstraint

final class CumulativeGenerator(pg: ProblemGenerator) extends Generator with LazyLogging {

  override def gen(constraint: CSPOMConstraint[Boolean])(implicit variables: VarMap) = {
    val Seq(s, d, r, b) = constraint.arguments

    val svars = cspom2concreteSeq(s).map(_.asVariable(pg)).toArray


    val dConc = cspom2concreteSeq(d)
    val rConc = cspom2concreteSeq(r)
    val bConc = cspom2concrete1D(b)


    val dVars = dConc.map(_.asVariable(pg)).toArray
    val rVars = rConc.map(_.asVariable(pg)).toArray
    val bVar = bConc.asVariable(pg)

    val profile = new Cumulative(svars, dVars, rVars, bVar)

//    val dConst = dConc.collect { case Const(i: Int) => i }.toArray
//    val rConst = rConc.collect { case Const(i: Int) => i }.toArray
//    val bConst = PartialFunction.condOpt(bConc) { case Const(i: Int) => i }

    val energy =
      new CumulativeEnergy(svars, dVars, rVars, bVar)


    Seq(profile, energy)

  }

}