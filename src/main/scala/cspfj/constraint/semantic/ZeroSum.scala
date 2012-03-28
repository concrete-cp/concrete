package cspfj.constraint.semantic;

import scala.annotation.tailrec

import cspfj.constraint.Constraint

import cspfj.problem.Domain
import cspfj.util.Interval
import cspfj.problem.Variable
import cspfj.util.Loggable

final class ZeroSum(
  val factors: Array[Int],
  scope: Array[Variable]) extends Constraint(scope)
  with Loggable {

  def checkValues(t: Array[Int]): Boolean =
    (0 until arity).map(i => t(i) * factors(i)).sum >= 0

  def getEvaluation = arity

  def shave() = {
    val bounds = scope.iterator.zip(factors.iterator).map {
      case (v, f) => v.dom.valueInterval * f
    } reduce { _ + _ }

    @tailrec
    def reviseVariable(i: Int = arity - 1, c: Boolean = false): Boolean =
      if (i < 0) c
      else {
        val f = factors(i)
        val dom = scope(i).dom

        val myBounds = dom.valueInterval * f

        val boundsf = Interval(bounds.lb - myBounds.lb, bounds.ub - myBounds.ub) / -f

        val ch = dom.intersectVal(boundsf)

        reviseVariable(i - 1, c || ch)
      }

    reviseVariable()
  }

  def revise(): Boolean = {

    var ch = false
    while (shave()) ch = true

    ch
  }

  override def toString = (scope, factors).zipped.map((v, f) => f + "." + v).mkString(" + ") + " >= 0"
  val simpleEvaluation = 3
}
