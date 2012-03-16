package cspfj.generator.constraint;

import cspfj.constraint.semantic.Mul
import cspfj.generator.FailedGenerationException
import cspfj.problem.{ Variable, Problem, IntDomain }
import cspom.constraint.CSPOMConstraint
import cspfj.constraint.AbstractConstraint
import cspfj.constraint.Residues
import cspfj.constraint.TupleEnumerator

final class ModGenerator(problem: Problem) extends AbstractGenerator(problem) {

  @throws(classOf[FailedGenerationException])
  def generate(constraint: CSPOMConstraint) = {
    val Seq(result, v0, v1) = constraint.scope map cspom2cspfj

    if (Seq(result, v0, v1) filter (_.dom == null) match {
      case Seq() => true
      case Seq(v) if v == result => {
        val values = AbstractGenerator.domainFrom(v0, v1, { _ % _ })
        v.dom = IntDomain(values: _*)
        true
      }

      case _ => false

    }) {
      addConstraint(new AbstractConstraint(Array(result, v0, v1)) with Residues with TupleEnumerator {
        def check =
          value(0) == value(1) % value(2)

      })
      true
    } else {
      false
    }

  }

}
