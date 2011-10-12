package cspfj.constraint.semantic;

import cspfj.constraint.AbstractConstraint
import cspfj.filter.RevisionHandler
import cspfj.problem.Domain
import cspfj.problem.Variable;
import cspfj.filter.RevisionHandler

object InInterval {
  def values(variable: Variable, lb: Int, ub: Int) =
    new InInterval(variable, variable.dom.closestGeq(lb), variable.dom.closestLeq(ub))

  def indices(variable: Variable, lb: Int, ub: Int) =
    new InInterval(variable, lb, ub)
}

final class InInterval(val variable: Variable, val lb: Int, val ub: Int)
  extends AbstractConstraint(Array(variable)) {

  val dom = variable.dom

  val getEvaluation = 0

  override def revise(revisator: RevisionHandler, reviseCount: Int): Boolean = {
    val removed = dom.removeTo(lb - 1) + dom.removeFrom(ub + 1);
    if (removed > 0) {
      if (dom.size == 0) {
        return false
      }
      revisator.revised(this, variable);

    }
    entail()
    true
  }

  def isConsistent(reviseCount: Int) = dom.indices(lb).takeWhile(_ <= ub).exists(dom.present)

  def check = {
    val value = this.value(0);
    lb <= value && value <= ub;
  }

  def toString = variable + " in [" + variable.dom.value(lb) + ", " + variable.dom.value(ub) + "]"

}
