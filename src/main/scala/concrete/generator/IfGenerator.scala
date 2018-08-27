package concrete.generator

import concrete.constraint.ReifiedConstraint
import concrete.constraint.linear.{Eq, EqACFast, EqReif}
import cspom.CSPOMConstraint

class IfGenerator(pg: ProblemGenerator) extends Generator {

  override def genFunctional(constraint: CSPOMConstraint[_], r: C2Conc)(implicit variables: VarMap): Seq[ReifiedConstraint] = {
    val result = r.asVariable(pg)

    val Seq(flag, thn, els) = constraint.arguments.map(Generator.cspom2concrete1D(_).asVariable(pg))

    Seq(
      new ReifiedConstraint(false, flag, new EqACFast(result, 0, thn)),
      new ReifiedConstraint(neg = true, flag, new EqACFast(result, 0, els))
    )

  }
}
