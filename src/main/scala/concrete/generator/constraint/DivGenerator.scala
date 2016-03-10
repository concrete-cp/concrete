package concrete.generator.constraint;

import Generator.cspom2concrete1D
import concrete.constraint.semantic.DivAC
import cspom.CSPOMConstraint

final object DivGenerator extends Generator {

  override def genFunctional(constraint: CSPOMConstraint[_], r: C2Conc)(implicit variables: VarMap) = {
    val result = r.asVariable
    val Seq(v0, v1) = constraint.arguments.map(c => cspom2concrete1D(c).asVariable)

    Seq(//new DivBC(v0, v1, result),
        new DivAC(v0, v1, result))
  }

}
