package concrete.generator;

import Generator.cspom2concrete
import concrete.constraint.semantic.Occurrence
import cspom.CSPOMConstraint

final object OccurrenceGenerator extends Generator {
  override def genFunctional(constraint: CSPOMConstraint[_], r: C2Conc)(implicit variables: VarMap) = {

    val Seq(value: C21D, Sequence(vars, _)) = constraint.arguments.map(cspom2concrete)

    Seq(new Occurrence(r.asVariable, value.asVariable, vars.map(_.asVariable).toArray))

  }

}
