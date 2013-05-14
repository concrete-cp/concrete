package cspfj.generator.constraint;

import scala.annotation.varargs

import cspfj.constraint.Constraint
import cspfj.constraint.Residues
import cspfj.constraint.TupleEnumerator
import cspfj.IntDomain
import cspfj.Problem
import cspom.constraint.FunctionalConstraint

final class ModGenerator(problem: Problem) extends AbstractGenerator(problem) {

  override def generateFunctional(constraint: FunctionalConstraint) = {
    val Seq(result, v0, v1) = constraint.scope map cspom2cspfj

    if (Seq(result, v0, v1) filter (_.dom.undefined) match {
      case Seq() => true
      case Seq(`result`) => {
        val values = AbstractGenerator.domainFrom(v0, v1, { _ % _ })
        result.dom = IntDomain(values: _*)
        true
      }

      case _ => false

    }) {
      addConstraint(new Constraint(Array(result, v0, v1)) with Residues with TupleEnumerator {
        def checkValues(t: Array[Int]) = t(0) == t(1) % t(2)
      })
      true
    } else {
      false
    }

  }

}
