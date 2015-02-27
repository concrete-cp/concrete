package concrete.generator.constraint;

import Generator.cspom2concreteVar
import concrete.constraint.semantic.Sum
import concrete.constraint.semantic.SumMode
import cspom.CSPOMConstraint
import concrete.constraint.semantic.SumBC
import concrete.constraint.semantic.SumAC

final object AddGenerator extends Generator {

  override def genFunctional(constraint: CSPOMConstraint[_], r: C2Conc)(implicit variables: VarMap) = {

    val result = cspom2concreteVar(constraint.result)

    val Seq(v0, v1) = constraint.arguments map cspom2concreteVar

    val factors = Array(-1, 1, 1)
    val scope = Array(result, v0, v1)

    Seq(new SumBC(0, factors, scope, SumMode.SumEQ), new SumAC(0, factors, scope, SumMode.SumEQ))

  }

}
