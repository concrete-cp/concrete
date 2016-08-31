package concrete.generator;

import Generator.cspom2concrete1D
import concrete.constraint.Constraint
import cspom.CSPOMConstraint
import cspom.variable.IntExpression

final class SetInGenerator(pg:ProblemGenerator) extends Generator {

  override def genFunctional(funcConstraint: CSPOMConstraint[_], r: C2Conc)(implicit variables: VarMap): Seq[Constraint] = {
    val result = r.asVariable(pg)
    val Seq(a, IntExpression.constSeq(constants)) = funcConstraint.arguments

    val variable = cspom2concrete1D(a).asVariable(pg)

    Seq(new concrete.constraint.semantic.SetIn(result, variable, constants.toSet))

  }

}
