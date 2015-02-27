package concrete.constraint.semantic

import com.typesafe.scalalogging.LazyLogging
import concrete.Contradiction
import concrete.Outcome
import concrete.ProblemState
import concrete.Variable
import concrete.constraint.Constraint
import concrete.constraint.Residues
import concrete.constraint.ScopeIds
import concrete.constraint.TupleEnumerator
import concrete.util.BitVector
import concrete.util.Interval

import concrete.Domain

object SumMode extends Enumeration {
  type SumMode = Value
  val SumLE = Value("le")
  val SumLT = Value("lt")
  val SumEQ = Value("eq")
  val SumNE = Value("ne")
}

import SumMode._

abstract class Sum(
  val constant: Int,
  val factors: Array[Int],
  scope: Array[Variable],
  val mode: SumMode.SumMode) extends Constraint(scope) {

  require(factors.forall(_ != 0), this)
  require(factors.size == scope.size)

  def check(t: Array[Int]): Boolean = {
    var i = arity - 1
    var sum = 0
    while (i >= 0) {
      sum += t(i) * factors(i)
      i -= 1
    }
    mode match {
      case SumLE => sum <= constant
      case SumLT => sum < constant
      case SumEQ => sum == constant
      case SumNE => sum != constant
    }
  }

}

trait SpanCalculator extends Sum {
  private val initBound = Interval(-constant, -constant)
  val doms = new Array[Domain](arity)

  def totalSpan(): Interval = {
    var bounds = initBound
    var i = arity - 1
    while (i >= 0) {
      bounds += doms(i).span * factors(i)
      i -= 1
    }
    bounds
  }

  def updateDoms(ps: ProblemState): Unit = {
    var i = arity - 1
    while (i >= 0) {
      doms(i) = ps.dom(scope(i))
      i -= 1
    }
  }

}

final class SumAC(
  constant: Int,
  factors: Array[Int],
  scope: Array[Variable],
  mode: SumMode.SumMode) extends Sum(constant, factors, scope, mode) with Residues with TupleEnumerator {

  def this(constant: Int, scope: Array[Variable], mode: SumMode.SumMode) =
    this(constant, Array.fill(scope.length)(1), scope, mode)

}

final class SumNE(
  constant: Int,
  factors: Array[Int],
  scope: Array[Variable]) extends Sum(constant, factors, scope, SumMode.SumNE) with SpanCalculator {
  def advise(problemState: concrete.ProblemState, pos: Int): Int = arity * 2
  def simpleEvaluation: Int = 3

  def this(constant: Int, scope: Array[Variable]) =
    this(constant, Array.fill(scope.length)(1), scope)

  def revise(ps: concrete.ProblemState): concrete.Outcome = {
    updateDoms(ps)
    var bounds = totalSpan

    var filtered: ProblemState = ps
    var change = true
    while (change) {
      change = false

      var i = arity - 1
      while (i >= 0) {
        val dom = doms(i)
        val span = dom.span
        val f = factors(i)

        val myBounds = span * f

        val thisBounds = Interval(bounds.lb - myBounds.lb, bounds.ub - myBounds.ub)

        val thisBoundsOnF = thisBounds / -f

        if (thisBoundsOnF.size == 1) {
          val newDom = dom.remove(thisBoundsOnF.lb)

          if (newDom ne dom) {
            if (newDom.isEmpty) {
              return Contradiction
            } else {
              filtered = filtered.updateDomNonEmpty(scope(i), newDom)
              doms(i) = newDom

              val newSpan = newDom.span

              if (newSpan != span) {
                bounds = thisBounds + newDom.span * f
                change = true
              }
            }
          }

        }

        i -= 1
      }

    }

    filtered
  }
}

final class SumBC(
  constant: Int,
  factors: Array[Int],
  scope: Array[Variable],
  mode: SumMode.SumMode) extends Sum(constant, factors, scope, mode) with SpanCalculator with LazyLogging {

  def this(constant: Int, scope: Array[Variable], mode: SumMode.SumMode) =
    this(constant, Array.fill(scope.length)(1), scope, mode)

  require(mode != SumNE, s"SumNE is not supported by SumBC ($this)")

  def advise(ps: ProblemState, p: Int) = arity * 2

  private def filter(dom: Domain, itv: Interval, neg: Boolean): Domain = mode match {
    case SumLE => if (neg) dom.removeUntil(itv.lb) else dom.removeAfter(itv.ub)
    case SumLT => if (neg) dom.removeTo(itv.lb) else dom.removeFrom(itv.ub)
    case SumEQ => dom & itv
  }

  def revise(ps: ProblemState): Outcome = {

    updateDoms(ps)

    var bounds = totalSpan

    var hasChanged = BitVector.empty
    var change = true
    while (change) {
      change = false

      var i = arity - 1
      while (i >= 0) {
        val dom = doms(i)

        val f = factors(i)

        val myBounds = dom.span * f

        val thisBounds = Interval(bounds.lb - myBounds.lb, bounds.ub - myBounds.ub)

        val newDom = filter(dom, thisBounds / -f, f < 0)

        if (newDom ne dom) {
          if (newDom.isEmpty) {
            return Contradiction
          } else {
            doms(i) = newDom
            change = true
            bounds = thisBounds + newDom.span * f
            hasChanged += i
          }
        }

        i -= 1
      }

    }

    var filtered: ProblemState = ps
    var i = hasChanged.nextSetBit(0)
    while (i >= 0) {
      filtered = filtered.updateDomNonEmpty(scope(i), doms(i))
      i = hasChanged.nextSetBit(i + 1)
    }
    filtered
  }

  override def toString() =
    (scope, factors).zipped.map((v, f) => f + "." + v).mkString(" + ") + s" $mode $constant"

  override def toString(ps: ProblemState) =
    (scope, factors).zipped.map((v, f) => f + "." + v.toString(ps)).mkString(" + ") + s" $mode $constant"

  val simpleEvaluation = 3
}
