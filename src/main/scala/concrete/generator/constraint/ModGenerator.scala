package concrete.generator.constraint;

import concrete.constraint.Constraint
import concrete.constraint.Residues
import concrete.constraint.TupleEnumerator
import concrete.IntDomain
import concrete.Problem
import concrete.Variable
import cspom.CSPOMConstraint
import Generator._

final object ModGenerator extends Generator {

  override def genFunctional(constraint: CSPOMConstraint, r: C2Conc)(implicit variables: VarMap) = {
    val Var(result) = r
    val Seq(v0, v1) = constraint.arguments map cspom2concreteVar

    if (Seq(result, v0, v1) filter (_.dom.undefined) match {
      case Seq() => true
      case Seq(`result`) => {
        val values = domainFrom(v0, v1, { _ % _ })
        result.dom = IntDomain(values: _*)
        true
      }

      case _ => false

    }) {
      Some(Seq(new Constraint(Array(result, v0, v1)) with Residues with TupleEnumerator {
        def checkValues(t: Array[Int]) = t(0) == t(1) % t(2)
      }))
    } else {
      None
    }

  }

}
