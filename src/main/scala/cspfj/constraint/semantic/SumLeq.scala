package cspfj.constraint.semantic;

import cspfj.constraint.AbstractConstraint
import cspfj.filter.RevisionHandler
import cspfj.problem.Domain
import cspfj.problem.Variable
import scala.annotation.tailrec
import cspfj.constraint.SimpleRemovals
import cspfj.util.Loggable

final class SumLeq(val bound: Int, scope: Array[Variable]) extends AbstractConstraint(scope)
with SimpleRemovals with Loggable {

  def check: Boolean = {

    @tailrec
    def c(i: Int, bound: Int): Boolean = {
      if (i >= arity) true else {
        val nb = bound - value(i)
        if (nb < 0) false else c(i + 1, nb)
      }
    }

    c(0, bound)
  }

  def getEvaluation = arity

  private def removeGt(value: Int, dom: Domain) = {
    val lb = dom.closestGeq(value)
    if (lb >= 0) {
      if (dom.value(lb) != value) {
        dom.removeFrom(lb) > 0;
      } else {
        dom.removeFrom(lb + 1) > 0;
      }
    } else false
  }

  def revise(revisator: RevisionHandler, reviseCount: Int): Boolean = {
    //info(this.toString)
    val newBound = bound - scope.iterator.map(_.dom.firstValue).sum
    if (newBound < 0) {
      return false;
    }

    var max = 0;
    for (v <- scope) {
      val dom = v.dom
      if (removeGt(newBound + dom.firstValue, dom)) {
        if (dom.size == 0) {
          return false;
        }
        revisator.revised(this, v);
      }
      max += dom.lastValue
    }

    if (max <= bound) {
      entail();
    }

    true;
  }

  override def toString = scope.mkString(" + ") + " <= " + bound
}
