package cspfj.constraint.semantic;

import scala.annotation.tailrec

import cspfj.constraint.Constraint

import cspfj.Domain
import cspfj.util.Interval
import cspfj.Variable
import cspfj.util.Loggable

final class ZeroSum(
  val factors: Array[Int],
  scope: Array[Variable]) extends Constraint(scope)
  with Loggable {

  val domFact = scope map (_.dom) zip factors toList

  def checkValues(t: Array[Int]): Boolean =
    (0 until arity).map(i => t(i) * factors(i)).sum >= 0

  def getEvaluation = arity

  def shave() = {

    val bounds = domFact map { case (d, f) => d.valueInterval * f } reduce (_ + _)

    //reduce (_ + _)
    //    val bounds = Interval(0, 0)

    domFact.foldLeft(false) {
      case (acc, (dom, f)) =>
        val myBounds = dom.valueInterval * f

        val boundsf = Interval(bounds.lb - myBounds.lb, bounds.ub - myBounds.ub) / -f

        dom.intersectVal(boundsf) || acc
    }

  }

  def revise() = {

    val bounds = domFact map { case (d, f) => d.valueInterval * f } reduce (_ + _)

    //reduce (_ + _)
    //    val bounds = Interval(0, 0)

    domFact.foldLeft(false) {
      case (acc, (dom, f)) =>
        val myBounds = dom.valueInterval * f

        val boundsf = Interval(bounds.lb - myBounds.lb, bounds.ub - myBounds.ub) / -f

        dom.intersectVal(boundsf) || acc
    }

  }

  override def toString = (scope, factors).zipped.map((v, f) => f + "." + v).mkString(" + ") + " >= 0"
  val simpleEvaluation = 3
}
