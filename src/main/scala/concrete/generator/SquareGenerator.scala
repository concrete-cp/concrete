package concrete
package generator;

import Generator.cspom2concrete1D
import concrete.constraint.semantic.SquareAC
import concrete.constraint.semantic.SquareBC
import cspom.CSPOMConstraint

final class SquareGenerator(pg: ProblemGenerator) extends Generator {

  override def genFunctional(constraint: CSPOMConstraint[_], result: C2Conc)(implicit variables: VarMap) = {
    val Seq(v0) = constraint.arguments.map(cspom2concrete1D(_).asVariable(pg))
    val r = result.asVariable(pg)
    Seq(
      new SquareBC(r, v0),
      new SquareAC(r, v0))
  }

}
