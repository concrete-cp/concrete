package concrete.generator;

import Generator.cspom2concrete
import concrete.constraint.semantic.Xor
import cspom.CSPOMConstraint

final class XorGenerator(pg: ProblemGenerator) extends Generator {
  override def gen(constraint: CSPOMConstraint[Boolean])(implicit varMap: VarMap) = {
    val vars = constraint.arguments
      .map(cspom2concrete)
      .map(_.asVariable(pg))

    Seq(new Xor(vars.toArray))
  }

}
