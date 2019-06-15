package concrete.generator

;

import concrete.constraint.semantic.FZSubcircuit
import concrete.generator.Generator._
import cspom.CSPOMConstraint
import cspom.variable.CSPOMSeq

final class SubCircuitGenerator(pg: ProblemGenerator) extends Generator {
  override def gen(constraint: CSPOMConstraint[Boolean])(implicit variables: VarMap): Seq[FZSubcircuit] = {
    val Seq(list: CSPOMSeq[_]) = constraint.arguments

    val vars = cspom2concreteSeq(list).map(_.asVariable(pg)).toArray

    Seq(new FZSubcircuit(vars))
  }

}
