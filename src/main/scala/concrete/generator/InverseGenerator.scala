package concrete.generator

import cspom.CSPOMConstraint
import concrete.constraint.semantic.Inverse
import Generator.cspom2concreteSeq
import concrete.constraint.semantic.AllDifferent2C

final class InverseGenerator(pg: ProblemGenerator, adg: AllDifferentGenerator) extends Generator {

  override def gen(constraint: CSPOMConstraint[Boolean])(implicit variables: VarMap) = {
    val Seq(x, y) = constraint.arguments.map(cspom2concreteSeq).map(_.toArray)

    val xvars = x.map(_.asVariable(pg))
    val yvars = y.map(_.asVariable(pg))
    
//    println(xvars.toSeq)
//    println(yvars.toSeq)

    Seq(
      new Inverse(xvars, yvars, 1, 1), new Inverse(yvars, xvars, 1, 1), new AllDifferent2C(xvars), new AllDifferent2C(yvars)) // ++ adg.generate(xvars) ++ adg.generate(yvars)

  }

}