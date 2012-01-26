package cspfj.constraint;

import scala.annotation.tailrec

import cspfj.problem.Variable
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
  def reviseVariable(position: Int): Boolean

  final def revise(reviseCount: Int): Boolean = {
    val skip = skipRevision(reviseCount);

    @tailrec
    def revise(singletons: Int, i: Int): Int = {
      if (i < 0) singletons
      else if (i == skip || !this.reviseVariable(i)) {

        if (scope(i).dom.size == 1) revise(singletons + 1, i - 1)
        else revise(singletons, i - 1)

      } else scope(i).dom.size match {
        case 0 => -1
        case 1 => revise(singletons + 1, i - 1)
        case _ => revise(singletons, i - 1)
      }
    }

    val singletons = revise(0, scope.length - 1)
    if (singletons < 0) {
      false
    } else {
      if (singletons >= arity - 1) {
        entail();
      }
      true;
    }
  }

}
