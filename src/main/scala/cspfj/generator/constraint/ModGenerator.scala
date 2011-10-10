package cspfj.generator.constraint;

import cspfj.constraint.semantic.Mul
import cspfj.exception.FailedGenerationException
import cspfj.problem.{ Variable, Problem, BitVectorDomain }
import cspom.constraint.CSPOMConstraint
import cspfj.constraint.AbstractAC3Constraint

final class ModGenerator(problem: Problem) extends AbstractGenerator(problem) {

  @throws(classOf[FailedGenerationException])
  def generate(constraint: CSPOMConstraint) = {
    val Seq(result, v0, v1) = constraint.scope map cspom2cspfj

    if (Seq(result, v0, v1) filter (_.domain == null) match {
      case Seq() => true
      case Seq(v) if v == result => {
        val values = AbstractGenerator.domainFrom(v0, v1, { _ % _ })
        v.domain = new BitVectorDomain(values: _*)
        true
      }

      case _ => false

    }) {
      addConstraint(new AbstractAC3Constraint(result, v0, v1) {
        def check = {
          getValue(0) == getValue(1) % getValue(2)
        }
      })
      true
    } else {
      false
    }

  }

}
