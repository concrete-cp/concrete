package concrete.generator

import concrete.constraint.semantic.{AllDifferent2C, Inverse}
import cspom.CSPOMConstraint


final class InverseGenerator(pg: ProblemGenerator, adg: AllDifferentGenerator) extends Generator {

  override def gen(constraint: CSPOMConstraint[Boolean])(implicit variables: VarMap) = {
    val Seq(x, y) = constraint.arguments.map(Generator.cspom2concreteIndexedSeq(_))


    val xvars = x.map(_._2.asVariable(pg)).toArray
    val yvars = y.map(_._2.asVariable(pg)).toArray

    val xRange = x.map(_._1)
    val yRange = y.map(_._1)

    require(xRange == (xRange.head to xRange.last))
    require(yRange == (yRange.head to yRange.last))

    val xOffset = xRange.head
    val yOffset = yRange.head

    //    println(xvars.toSeq)
    //    println(yvars.toSeq)

    if (yvars.length == xvars.length) {

      Seq(
        new Inverse(xvars, yvars, xOffset, yOffset),
        new Inverse(yvars, xvars, yOffset, xOffset),
        new AllDifferent2C(xvars),
        new AllDifferent2C(yvars))
    } else {
      Seq(
        new Inverse(xvars, yvars, xOffset, yOffset)
      )
    }

  }

}