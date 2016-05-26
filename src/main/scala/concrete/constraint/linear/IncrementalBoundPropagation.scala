package concrete.constraint.linear

import IncrementalBoundPropagation.PFiltered
import Linear.maxTimes
import Linear.minTimes
import concrete.Domain
import concrete.IntDomain
import concrete.ProblemState
import concrete.constraint.StatefulConstraint
import concrete.util.Interval
import concrete.util.Math
import cspom.util.BitVector
import concrete.Outcome

object IncrementalBoundPropagation {
  sealed trait POutcome
  case object PContradiction extends POutcome
  case class PFiltered(changed: BitVector, entailed: Boolean, f: Interval, vars: BitVector, max: Int) extends POutcome
}

trait IncrementalBoundPropagation extends Linear with StatefulConstraint[(Array[Domain], Interval, BitVector, Int)] {
  import IncrementalBoundPropagation._

  def is: Array[Int]

  def updateF(ps: ProblemState, mod: BitVector) = {
    val (doms, f, vars, maxI) = ps(this)

    val newDoms = doms.clone

    var boundchange = false

    var newF = f
    var newVars = vars

    for (p <- mod) {
      val nd = ps.dom(scope(p))

      newDoms(p) = nd

      val oldspan = doms(p).span
      val newspan = nd.span

      if (newspan != oldspan) {
        boundchange = true

        val factor = factors(p)

        newF = newF.shrink(oldspan * factor) + (newspan * factor)

        if (nd.size == 1) {
          newVars -= p
        }
      }
    }

    (newDoms, newF, newVars, maxI, boundchange)

  }

  def initData(ps: ProblemState) = {
    val doms = ps.doms(scope) //Array.tabulate[Domain](arity)(p => IntDomain.ofInterval(-1000000, 1000000)) // ps.dom(scope(p)))
    val f = (doms, factors).zipped.map(_.span * _).reduce(_ + _) - constant
    val maxI = size(doms.head, factors.head)
    val vars = BitVector.filled(arity).filter(p => !doms(p).isAssigned)

    proceed(ps, doms, f, vars, maxI)
    // println(s"$mode, arity $arity, $f")
    //ps.updateState(this, (doms, f, vars, maxI§K))
  }

  def proceed(ps: ProblemState, doms: Array[Domain], f: Interval, vars: BitVector, max: Int): Outcome

  //
  //  @annotation.tailrec
  //  final def realMax(i: Int, doms: Array[Domain], factors: Array[Int], vars: BitVector, max: Int): Int = {
  //    if (i < 0 || is(i) <= max) max
  //    else realMax(
  //      vars.nextSetBit(i + 1),
  //      doms,
  //      factors,
  //      vars,
  //      math.max(max, size(doms(i), factors(i))))
  //
  //  }
  //

  def realMax(i: Int, doms: Array[Domain], factors: Array[Int], vars: BitVector, max: Int): Int = {
    if (i < 0) max else
      math.max(max, is(i))
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
      /* Entailed */
      val rm = if (i < 0) max else math.max(max, is(i))
      //require(i < 0 || rm <= is(i))
      PFiltered(hasChanged, true, f, vars, rm)
      //if (i < 0) max else math.max(max, is(i)))
    } else if (f.lb > 0) {
      PContradiction
    } else if (i < 0) {
      PFiltered(hasChanged, false, f, vars, max)
    } else if (is(i) <= -f.lb) {
      /* Short-circuit */
      val rm = math.max(max, is(i))
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
