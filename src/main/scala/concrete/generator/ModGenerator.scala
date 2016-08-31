package concrete.generator;

import Generator.cspom2concrete1D
import concrete.constraint.semantic.ModAC
import concrete.constraint.semantic.ModBC
import cspom.CSPOMConstraint

final class ModGenerator(pg:ProblemGenerator) extends Generator {

  override def genFunctional(constraint: CSPOMConstraint[_], r: C2Conc)(implicit variables: VarMap) = {
    val result = r.asVariable(pg)
    val Seq(v0, v1) = constraint.arguments.map(c => cspom2concrete1D(c).asVariable(pg))

    Seq(new ModBC(v0, v1, result), new ModAC(v0, v1, result))
  }

}
