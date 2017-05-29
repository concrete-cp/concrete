package concrete.generator

import concrete.constraint.semantic.{AllDifferent2C, Channel}
import cspom.CSPOMConstraint


final class ChannelGenerator(pg: ProblemGenerator, adg: AllDifferentGenerator) extends Generator {

  override def gen(constraint: CSPOMConstraint[Boolean])(implicit variables: VarMap) = {
    val Seq(x) = constraint.arguments.map(Generator.cspom2concreteIndexedSeq(_))


    val xvars = x.map(_._2.asVariable(pg)).toArray

    val xRange = x.map(_._1)

    require(xRange == (xRange.head to xRange.last))

    val xOffset = xRange.head

    //    println(xvars.toSeq)
    //    println(yvars.toSeq)

    Seq(
      new Channel(xvars, xOffset),
      new AllDifferent2C(xvars))

  }

}