package concrete.generator.constraint;

import Generator.cspom2concreteVar
import concrete.constraint.semantic.AddAC
import concrete.constraint.semantic.Sum
import concrete.constraint.semantic.SumMode
import cspom.CSPOMConstraint

final object AddGenerator extends Generator {

  override def genFunctional(constraint: CSPOMConstraint[_], r: C2Conc)(implicit variables: VarMap) = {

    val result = cspom2concreteVar(constraint.result)

    val Seq(v0, v1) = constraint.arguments map cspom2concreteVar

    Seq(new Sum(0, Array(1, 1, -1), Array(result, v0, v1), SumMode.SumEQ), new AddAC(result, v0, v1, true))

  }

}
