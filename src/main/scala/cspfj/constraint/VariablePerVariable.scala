package cspfj.constraint;

import cspfj.filter.RevisionHandler
import cspfj.problem.Variable;
import cspfj.util.Loggable

/**
 * A constraint that can be revised one variable at a time
 */
trait VariablePerVariable extends VariableGrainedRemovals with Loggable {

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
    for ((variable, i) <- scope.zipWithIndex) {
      fine(this.toString + ", " + i + " (" + removals.toSeq + " -> skip " + skip + ")")
      assert(skip != i || !revise(i), "Should not skip " + this + ", " + i)
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
