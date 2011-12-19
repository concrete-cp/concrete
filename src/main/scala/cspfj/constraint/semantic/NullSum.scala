package cspfj.constraint.semantic;

import cspfj.constraint.AbstractConstraint
import cspfj.filter.RevisionHandler
import cspfj.problem.Domain
import cspfj.problem.Variable
import scala.annotation.tailrec
import cspfj.constraint.SimpleRemovals
import cspfj.util.Loggable
import cspfj.problem.Interval

final class NullSum(
  val factors: Array[Int],
  scope: Array[Variable]) extends AbstractConstraint(scope)
  with SimpleRemovals with Loggable {

  def check: Boolean =
    (0 until arity).map(i => value(i) * factors(i)).sum >= 0

  def getEvaluation = arity

  def revise(revisator: RevisionHandler, reviseCount: Int): Boolean = {
    //info(this.toString)
    //    val sum = scope.iterator.zip(factors.iterator).map {
    //      case (v, f) => v.dom.valueInterval * f
    //    } reduce { _ + _ }

    //    if (sum.lb >= 0) {
    //      entail()
    //      true
    //    } else if (sum.ub < 0) {
    //      false
    //    } else {

    val bounds = scope.iterator.zip(factors.iterator).map {
      case (v, f) => v.dom.valueInterval * f
    } reduce { _ + _ }

    @tailrec
    def revise(i: Int): Boolean = {
      if (i < 0) {
        true
      } else {
        val v = scope(i)
        val f = factors(i)
        val dom = v.dom

        val myBounds = v.dom.valueInterval * f

        val boundsf = Interval(bounds.lb - myBounds.lb, bounds.ub - myBounds.ub) / -f

        val removed = dom.intersectVal(boundsf)

        if (removed > 0) {
          if (dom.size == 0) {
            false;
          } else {
            revisator.revised(this, v);
            revise(i - 1)
          }
        } else revise(i - 1)
      }
    }

    revise(arity - 1)
  }

  override def toString = (scope, factors).zipped.map((v, f) => f + "." + v).mkString(" + ") + " >= 0"
}
