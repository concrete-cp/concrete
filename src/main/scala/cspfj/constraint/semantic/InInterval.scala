package cspfj.constraint.semantic;

import cspfj.constraint.AbstractConstraint
import cspfj.constraint.SimpleRemovals
import cspfj.problem.Domain
import cspfj.problem.Variable

object InInterval {
  def values(variable: Variable, lb: Int, ub: Int) =
    new InInterval(variable, variable.dom.closestGeq(lb), variable.dom.closestLeq(ub))

  def indices(variable: Variable, lb: Int, ub: Int) =
    new InInterval(variable, lb, ub)
}

final class InInterval(val variable: Variable, val lb: Int, val ub: Int)
  extends AbstractConstraint(Array(variable)) with SimpleRemovals {

  val dom = variable.dom

  override val getEvaluation = 0.0

  override def revise(reviseCount: Int): Boolean = {
    val removed = dom.removeTo(lb - 1) + dom.removeFrom(ub + 1);

    if (dom.size == 0) false
    else {
      entail()
      true
    }
  }

  override def isConsistent(reviseCount: Int) = dom.indices(lb).takeWhile(_ <= ub).exists(dom.present)

  def check = {
    val value = this.value(0);
    lb <= value && value <= ub;
  }

  override def toString = variable + " in [" + variable.dom.value(lb) + ", " + variable.dom.value(ub) + "]"

}
