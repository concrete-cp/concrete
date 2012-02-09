package cspfj.constraint.semantic;

import cspfj.constraint.AbstractConstraint
import cspfj.problem.Domain
import cspfj.problem.Variable;

object NotInInterval {
  def values(variable: Variable, lb: Int, ub: Int) =
    new NotInInterval(variable, variable.dom.closestGeq(lb), variable.dom.closestLeq(ub))

  def indices(variable: Variable, lb: Int, ub: Int) =
    new NotInInterval(variable, lb, ub)
}

/**
 * Constraint: variable \notin [lb, ub] 
 * lb and ub are domain indices!
 */
final class NotInInterval(val variable: Variable, val lb: Int, val ub: Int)
  extends AbstractConstraint(Array(variable)) {

  val getEvaluation = 1

  def revise(reviseCount: Int) {

    variable.dom.indices(lb).takeWhile(_ <= ub).foreach { i =>
      variable.dom.remove(i)
    }

    entail();

  }

  override def isConsistent(reviseCount: Int) =
    variable.dom.first < lb || variable.dom.last > ub;

  def check = {
    val value = this.value(0);
    value < lb || ub < value;
  }

  override def toString = variable + " notin [" + variable.dom.value(lb) + ", " + variable.dom.value(ub) + "]"

}
