package concrete.generator;

import Generator.cspom2concrete
import concrete.constraint.semantic.AtLeast
import cspom.CSPOMConstraint

final class AtLeastGenerator(pg: ProblemGenerator) extends Generator {
  override def gen(constraint: CSPOMConstraint[Boolean])(implicit variables: VarMap) = {

    val Seq(count: C21D, value: C21D, Sequence(vars, _)) = constraint.arguments.map(cspom2concrete)

    Seq(new AtLeast(count.asVariable(pg), value.asVariable(pg), vars.map(_.asVariable(pg)).toArray))

  }

}
