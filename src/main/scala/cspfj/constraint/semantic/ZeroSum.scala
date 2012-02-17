package cspfj.constraint.semantic;

import scala.annotation.tailrec

import cspfj.constraint.AbstractConstraint

import cspfj.problem.Domain
import cspfj.problem.Interval
import cspfj.problem.Variable
import cspfj.util.Loggable

final class ZeroSum(
  val factors: Array[Int],
  scope: Array[Variable]) extends AbstractConstraint(scope)
  with Loggable {

  def check: Boolean =
    (0 until arity).map(i => value(i) * factors(i)).sum >= 0

  def getEvaluation = arity

  def revise() {
    val bounds = scope.iterator.zip(factors.iterator).map {
      case (v, f) => v.dom.valueInterval * f
    } reduce { _ + _ }

    @tailrec
    def reviseVariable(i: Int): Boolean = {
      if (i < 0) true
      else {
        val f = factors(i)
        val dom = scope(i).dom

        val myBounds = dom.valueInterval * f

        val boundsf = Interval(bounds.lb - myBounds.lb, bounds.ub - myBounds.ub) / -f

        dom.intersectVal(boundsf)

        reviseVariable(i - 1)
      }
    }

    reviseVariable(arity - 1)
  }

  override def toString = (scope, factors).zipped.map((v, f) => f + "." + v).mkString(" + ") + " >= 0"
}
