package concrete.generator.constraint;

import concrete.constraint.Constraint
import concrete.constraint.Residues
import concrete.constraint.TupleEnumerator
import concrete.IntDomain
import concrete.Problem
import concrete.Variable
import cspom.CSPOMConstraint
import Generator._
import concrete.constraint.semantic.ModAC
import concrete.constraint.semantic.ModBC
import concrete.constraint.semantic.DivAC
import concrete.constraint.semantic.DivBC

final object DivGenerator extends Generator {

  override def genFunctional(constraint: CSPOMConstraint[_], r: C2Conc)(implicit variables: VarMap) = {
    val result = r.asVariable
    val Seq(v0, v1) = constraint.arguments.map(c => cspom2concrete1D(c).asVariable)

    Seq(//new DivBC(v0, v1, result),
        new DivAC(v0, v1, result))
  }

}
