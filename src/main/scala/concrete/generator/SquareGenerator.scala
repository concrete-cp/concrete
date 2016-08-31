package concrete.generator;

import Generator.cspom2concreteVar
import concrete.constraint.semantic.SquareAC
import concrete.constraint.semantic.SquareBC
import cspom.CSPOMConstraint

final object SquareGenerator extends Generator {

  override def genFunctional(constraint: CSPOMConstraint[_], result: C2Conc)(implicit variables: VarMap) = {
    val Seq(v0) = constraint.arguments map cspom2concreteVar
    val Var(r) = result
    Seq(new SquareBC(r, v0), new SquareAC(r, v0))
  }

}
