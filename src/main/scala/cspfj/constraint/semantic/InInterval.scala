package cspfj.constraint.semantic;

import cspfj.constraint.AbstractConstraint
import cspfj.problem.Domain
import cspfj.problem.Variable

object InInterval {
  def values(variable: Variable, lb: Int, ub: Int) =
    new InInterval(variable, variable.dom.closestGeq(lb), variable.dom.closestLeq(ub))

  def indices(variable: Variable, lb: Int, ub: Int) =
    new InInterval(variable, lb, ub)
}

/**
 * Constraint: variable \in [lb, ub]
 * lb and ub are domain indices!
 */
final class InInterval(val variable: Variable, val lb: Int, val ub: Int)
  extends AbstractConstraint(Array(variable)) {

  val dom = variable.dom

  override val getEvaluation = 1

  def revise() {
    dom.removeTo(lb - 1)
    dom.removeFrom(ub + 1);

    entail()
  }

  override def isConsistent() = dom.indices(lb).takeWhile(_ <= ub).exists(dom.present)

  def check = {
    val value = this.value(0);
    lb <= value && value <= ub;
  }

  override def toString = variable + " in [" + variable.dom.value(lb) + ", " + variable.dom.value(ub) + "]"
  val simpleEvaluation = 1
}
