package cspfj.constraint.semantic;

import cspfj.constraint.AbstractConstraint;
import cspfj.filter.RevisionHandler;
import cspfj.problem.Domain;
import cspfj.problem.Variable;

object NotInInterval {
  def values(variable: Variable, lb: Int, ub: Int) =
    new NotInInterval(variable, variable.dom.closestGeq(lb), variable.dom.closestLeq(ub))

  def indices(variable: Variable, lb: Int, ub: Int) =
    new NotInInterval(variable, lb, ub)
}

final class NotInInterval(val variable: Variable, val lb: Int, val ub: Int)
  extends AbstractConstraint(Array(variable)) {

  val getEvaluation = 0

  def revise(revisator: RevisionHandler, reviseCount: Int): Boolean = {
    var changed = false;
    variable.dom.indices(lb).takeWhile(_ <= ub).foreach { i =>
      variable.dom.remove(i)
      changed = true
    }

    if (changed) {
      if (variable.dom.size == 0) {
        return false;
      }
      revisator.revised(this, variable);
    }
    entail();
    true;
  }

  def isConsistent(reviseCount: Int) =
    variable.dom.first < lb || variable.dom.last > ub;

  def check = {
    val value = this.value(0);
    value < lb || ub < value;
  }

  def toString = variable + " notin [" + variable.dom.value(lb) + ", " + variable.dom.value(ub) + "]"

}
