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
  def reviseVariable(position: Int): Boolean

  def revise(modified: Seq[Int]) = {
    @tailrec
    def reviseNS(i: Int, c: Boolean): Boolean =
      if (i < 0) c
      else {
        val ch = this.reviseVariable(i)
        reviseNS(i - 1, c || ch)
      }

    @tailrec
    def reviseS(i: Int, skip: Int, c: Boolean): Boolean =
      if (i < 0) c
      else if (i == skip) reviseNS(i - 1, c)
      else {
        val ch = this.reviseVariable(i)
        reviseS(i - 1, skip, c || ch)
      }

    val c = modified match {
      case Seq() => false
      case Seq(s) => reviseS(scope.length - 1, s, false)
      case _ => reviseNS(scope.length - 1, false)
    }

    if (scope.count(_.dom.size == 1) >= arity - 1) {
      entail();
    }

    c

  }

}
