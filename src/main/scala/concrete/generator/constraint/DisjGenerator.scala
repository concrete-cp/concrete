package concrete.generator.constraint;

import concrete.constraint.semantic.Disjunction
import concrete.{ Variable, Problem }
import cspom.CSPOMConstraint
import cspom.variable.CSPOMVariable
import cspom.variable.CSPOMTrue
import cspom.variable.CSPOMFalse
import cspom.variable.BoolVariable

final class DisjGenerator(problem: Problem) extends AbstractGenerator(problem) {

  override def gen(gC: CSPOMConstraint) = {
    require(gC.function == 'or, "Unexpected constraint " + gC)
    val scope = gC.arguments map cspom2concreteVar

    scope foreach AbstractGenerator.booleanDomain
    val params: IndexedSeq[Boolean] = gC.params.get("revsign") match {
      case Some(p: Seq[Boolean]) => p.toIndexedSeq
      case None => IndexedSeq.fill(scope.size)(false)
      case p: Any => throw new IllegalArgumentException(s"Parameters for disjunction must be a sequence of boolean values, not '$p'")
    }

    addConstraint(new Disjunction(scope.toArray, params))
    true
  }

}
