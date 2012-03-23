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
  def reviseVariable(position: Int, modified: Seq[Int]): Boolean

  def revise(modified: Seq[Int]) = {
    @tailrec
    def reviseNS(i: Int = arity - 1, c: Boolean = false): Boolean =
      if (i < 0) c
      else {
        val ch = this.reviseVariable(i, modified)
        reviseNS(i - 1, c || ch)
      }

    @tailrec
    def reviseS(skip: Int, i: Int = arity - 1, c: Boolean = false): Boolean =
      if (i < 0) c
      else if (i == skip) reviseNS(i - 1, c)
      else {
        val ch = this.reviseVariable(i, modified)
        reviseS(skip, i - 1, c || ch)
      }

    val c = modified match {
      case Seq() => false
      case Seq(s) => reviseS(s)
      case _ => reviseNS()
    }

    entailCheck(c)
    c

  }

  def entailCheck(c: Boolean) {
    if (c && scope.count(_.dom.size == 1) >= arity - 1) {
      entail();
    }
  }

}
