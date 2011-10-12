package cspfj.constraint;

import cspfj.filter.RevisionHandler;
import cspfj.problem.Variable;

/**
 * A constraint that can be revised one variable at a time
 */
trait VariablePerVariable extends ArcGrainedConstraint {

  /**
   * Try to filter values from variable getVariable(position).
   *
   * @param position
   * @return true iff any value has been removed
   */
  def revise(position: Int): Boolean

  final def revise(revisator: RevisionHandler, reviseCount: Int): Boolean = {
    var singletons = 0;
    val skip = skipRevision(reviseCount);
    for (i <- (0 to arity)) {
      val variable = scope(i);
      // assert (!variable.isAssigned() && skipRevision(i)) ? !revise(i)
      // : true : "Should not skip " + this + ", " + i;
      if (i != skip && revise(i)) {
        if (variable.dom.size == 0) {
          return false;
        }
        revisator.revised(this, variable);
      }
      if (variable.dom.size == 1) {
        singletons += 1;
      }
    }
    if (singletons >= arity - 1) {
      entail();
    }
    true;
  }

}
