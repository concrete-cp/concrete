package concrete.generator.constraint;

import concrete.constraint.semantic.Disjunction
import concrete.{ Variable, Problem }
import cspom.CSPOMConstraint
import cspom.variable.CSPOMVariable
import cspom.variable.CSPOMConstant
import cspom.variable.BoolVariable

import Generator._

final object DisjGenerator extends Generator {

  override def gen(gC: CSPOMConstraint[Boolean])(implicit variables: VarMap) = {
    require(gC.function == 'or, "Unexpected constraint " + gC)
    val scope = gC.arguments map cspom2concreteVar

    val params: IndexedSeq[Boolean] = gC.params.get("revsign") match {
      case Some(p: Seq[Boolean]) => p.toIndexedSeq
      case None => IndexedSeq.fill(scope.size)(false)
      case p: Any => throw new IllegalArgumentException(s"Parameters for disjunction must be a sequence of boolean values, not '$p'")
    }

    Seq(new Disjunction(scope.toArray, params))
  }

}
