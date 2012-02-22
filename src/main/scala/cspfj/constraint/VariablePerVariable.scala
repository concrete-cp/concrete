package cspfj.constraint;

import scala.annotation.tailrec

import cspfj.problem.Variable
import cspfj.util.Loggable

/**
 * A constraint that can be revised one variable at a time
 */
trait VariablePerVariable extends Constraint with Removals with Loggable {

  /**
   * Try to filter values from variable getVariable(position).
   *
   * @param position
   * @return true iff any value has been removed
   */
  def reviseVariable(position: Int)

  final def revise(modified: Seq[Int]) {
    @tailrec
    def reviseNS(i: Int) {

      if (i >= 0) {
        this.reviseVariable(i)
        reviseNS(i - 1)
      }

    }

    @tailrec
    def reviseS(i: Int, skip: Int) {

      if (i == skip) reviseNS(i - 1)
      else if (i > 0) {
        this.reviseVariable(i)
        reviseS(i - 1, skip)
      }

    }

    modified match {
      case Seq() =>
      case Seq(s) => reviseS(scope.length - 1, s)
      case _ => reviseNS(scope.length - 1)
    }

    if (scope.count(_.dom.size == 1) >= arity - 1) {
      entail();
    }

  }

}
