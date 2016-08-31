package concrete.generator;

import Generator.cspom2concrete1D
import concrete.constraint.semantic.DivAC
import cspom.CSPOMConstraint
import concrete.constraint.semantic.DivBC

final class DivGenerator(pg: ProblemGenerator) extends Generator {

  override def genFunctional(constraint: CSPOMConstraint[_], r: C2Conc)(implicit variables: VarMap) = {
    val result = r.asVariable(pg)
    val Seq(v0, v1) = constraint.arguments.map(c => cspom2concrete1D(c).asVariable(pg))

    Seq(new DivBC(v0, v1, result),
      new DivAC(v0, v1, result))
  }

}
