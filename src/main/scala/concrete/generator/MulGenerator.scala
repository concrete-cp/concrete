package concrete.generator;

import Generator.cspom2concrete1D
import concrete.constraint.semantic.MulAC
import concrete.constraint.semantic.MulBC
import cspom.CSPOMConstraint

final class MulGenerator(pg: ProblemGenerator) extends Generator {

  override def genFunctional(constraint: CSPOMConstraint[_], r: C2Conc)(implicit variables: VarMap) = {
    val rr = r.asInstanceOf[C21D]
    val Seq(vv0, vv1) = constraint.arguments map cspom2concrete1D

    val result = rr.asVariable(pg)
    val v0 = vv0.asVariable(pg)
    val v1 = vv1.asVariable(pg)

    val bc = Seq(new MulBC(result, v0, v1))

    val a = result.initDomain.size
    val b = v0.initDomain.size
    val c = v1.initDomain.size

    if (a * b + a * c + b * c < 10000) {
      new MulAC(result, v0, v1, true) +: bc
    } else {
      bc
    }

  }

}
