package concrete.constraint.linear

import com.typesafe.scalalogging.LazyLogging

import IncrementalBoundPropagation.PContradiction
import IncrementalBoundPropagation.PFiltered
import IncrementalBoundPropagation.POutcome
import LinearLe.maxTimes
import LinearLe.minTimes
import concrete.Domain
import concrete.Outcome
import concrete.ProblemState
import concrete.Variable
import concrete.constraint.StatefulConstraint
import concrete.util.Interval
import concrete.util.Math
import cspom.util.BitVector

object IncrementalBoundPropagation {
  sealed trait POutcome
  case object PContradiction extends POutcome
  case class PFiltered(changed: BitVector, entailed: Boolean, f: Interval, vars: BitVector, max: Int) extends POutcome

}

trait IncrementalBoundPropagation extends Linear with StatefulConstraint[(Array[Domain], Interval, BitVector, Int)] {

  import LinearLe._
  import IncrementalBoundPropagation._

  def is: Array[Int]

  def filter(changed: BitVector, doms: Array[Domain], ps: ProblemState): Outcome = {
    var filtered = ps
    var i = changed.nextSetBit(0)
    while (i >= 0) {
      filtered = filtered.updateDomNonEmpty(scope(i), doms(i))
      i = changed.nextSetBit(i + 1)
    }
    filtered
  }

  def updateF(ps: ProblemState, mod: Seq[Int]) = {
    var (doms, f, vars, maxI) = ps(this)

    val newDoms = doms.clone

    var i = mod.length - 1

    while (i >= 0) {
      val p = mod(i)

      val nd = ps.dom(scope(p))

      if (nd != doms(p)) {
        newDoms(p) = nd

        val factor = factors(p)

        f = f.shrink(doms(p).span * factor) + (nd.span * factor)

        if (nd.size == 1) {
          vars -= p
        }
      }
      i -= 1

    }

    (newDoms, f, vars, maxI)
  }

  def initData(ps: ProblemState) = {
    val doms = Array.tabulate(arity)(p => ps.dom(scope(p)))
    val f = (doms, factors).zipped.map(_.span * _).reduce(_ + _) - constant
    val maxI = size(doms.head, factors.head)
    val vars = BitVector.filled(arity).filter(p => !doms(p).isAssigned)

    // println(s"$mode, arity $arity, $f")
    ps.updateState(this, (doms, f, vars, maxI))
  }

  //  def maxI(doms: Array[Domain]): Int = {
  //    var max = 0
  //    var p = 0
  //    while (p < arity && max < is(p)) {
  //      max = math.max(max, doms(p).span.size * math.abs(factors(p)))
  //      p += 1
  //    }
  //    max
  //  }

  @annotation.tailrec
  final def realMax(i: Int, doms: Array[Domain], factors: Array[Int], vars: BitVector, max: Int): Int = {
    if (i < 0 || is(i) <= max) max
    else realMax(
      vars.nextSetBit(i + 1),
      doms,
      factors,
      vars,
      math.max(max, size(doms(i), factors(i))))

  }

  
  protected def size(dom: Domain, factor: Int): Int = {
    math.abs(factor) * (dom.last - dom.head)
  }

  @annotation.tailrec
  final def processUB(
    i: Int,
    vars: BitVector,
    doms: Array[Domain],
    f: Interval,
    factors: Array[Int],
    ps: ProblemState,
    hasChanged: BitVector = BitVector.empty,
    max: Int = 0): POutcome = {

    if (f.ub <= 0) {
      /** Entailed */
      val rm = realMax(i, doms, factors, vars, max)
      //require(i < 0 || rm <= is(i))
      PFiltered(hasChanged, true, f, vars, rm)
      //if (i < 0) max else math.max(max, is(i)))
    } else if (f.lb > 0) {
      PContradiction
    } else if (i < 0) {
      PFiltered(hasChanged, false, f, vars, max)
    } else if (is(i) <= -f.lb) {
      /** Short-circuit */
      val rm = realMax(i, doms, factors, vars, max)
      //require(rm <= is(i), (max, rm, doms.toSeq, factors.toSeq, is.toSeq))
      PFiltered(hasChanged, false, f, vars, rm)
    } else {
      val dom = doms(i)

      val fact = factors(i)

      val size = this.size(dom, fact)

      if (size <= -f.lb) {
        processUB(vars.nextSetBit(i + 1), vars, doms, f,
          factors, ps, hasChanged, math.max(max, size))
      } else {

        val thisBounds = f.lb - minTimes(dom, fact)

        val newDom = if (fact < 0) dom.removeUntil(Math.ceilDiv(thisBounds, -fact)) else dom.removeAfter(Math.floorDiv(thisBounds, -fact))
        LinearLe.shaves += 1

        val newMax = math.max(max, this.size(newDom, fact))

        doms(i) = newDom //val newDoms = doms.updated(i, newDom)
        val newF = Interval(thisBounds, f.ub - maxTimes(dom, fact)) + newDom.span * fact
        if (newDom.isAssigned) {
          val newVars = vars - i
          processUB(newVars.nextSetBit(i + 1), newVars, doms, newF, factors, ps, hasChanged + i, newMax)
        } else {
          processUB(vars.nextSetBit(i + 1), vars, doms, newF, factors, ps, hasChanged + i, newMax)
        }
      }

    }
  }

}
