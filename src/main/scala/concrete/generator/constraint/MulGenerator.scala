package concrete.generator.constraint;

import Generator.cspom2concrete1D
import concrete.constraint.semantic.MulAC
import concrete.constraint.semantic.MulBC
import cspom.CSPOMConstraint

final object MulGenerator extends Generator {

  override def genFunctional(constraint: CSPOMConstraint[_], r: C2Conc)(implicit variables: VarMap) = {
    val rr = r.asInstanceOf[C21D]
    val Seq(vv0, vv1) = constraint.arguments map cspom2concrete1D

    val result = rr.asVariable
    val v0 = vv0.asVariable
    val v1 = vv1.asVariable

    val bc = Seq(new MulBC(result, v0, v1))
    if (result.initDomain.size + v0.initDomain.size + v1.initDomain.size < 1000) {
      new MulAC(result, v0, v1, true) +: bc
    } else {
      bc
    }

  }

}
