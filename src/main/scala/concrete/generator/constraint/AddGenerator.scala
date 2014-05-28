package concrete.generator.constraint;

import Generator._
import concrete.Variable
import concrete.constraint.semantic.AddAC
import concrete.constraint.semantic.AddBC
import cspom.CSPOMConstraint

final object AddGenerator extends Generator {

  override def genFunctional(constraint: CSPOMConstraint[_], r: C2Conc)(implicit variables: VarMap) = {

    val result = cspom2concreteVar(constraint.result)

    val Seq(v0, v1) = constraint.arguments map cspom2concreteVar

    allDefinedOption(result, v0, v1) {
      Seq(new AddBC(result, v0, v1), new AddAC(result, v0, v1, true))
    }
  }

}
