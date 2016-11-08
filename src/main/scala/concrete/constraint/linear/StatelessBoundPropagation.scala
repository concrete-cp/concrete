package concrete.constraint.linear

import concrete.util.Interval
import concrete.Domain
import concrete.ProblemState
import cspom.util.BitVector
import Linear.maxTimes
import Linear.minTimes
import concrete.util.Math

object StatelessBoundPropagation {
  sealed trait POutcome
  case object PContradiction extends POutcome
  case class PFiltered(changed: BitVector, entailed: Boolean, f: Interval) extends POutcome
}

trait StatelessBoundPropagation extends Linear {
  import StatelessBoundPropagation._
  protected val doms = new Array[Domain](arity)
  protected val initBound = -Interval(constant, constant)
  protected def is: Array[Int]

  protected def updateF(ps: ProblemState) = {
    var f = initBound
    var p = arity - 1
    var maxI = 0
    while (p >= 0) {
      val dom = ps.dom(scope(p))

      doms(p) = dom

      val span = dom.span * factors(p)

      f += span

      maxI = math.max(maxI, span.ub - span.lb)

      p -= 1
    }

    (f, maxI)

  }

  @annotation.tailrec
  final def processUB(
    p: Int,
    f: Interval,
    factors: Array[Int],
    ps: ProblemState,
    hasChanged: BitVector = BitVector.empty): POutcome = {

    if (f.ub <= 0) {
      /* Entailed */
      PFiltered(hasChanged, true, f)
    } else if (f.lb > 0) {
      PContradiction
    } else if (p >= arity || is(p) <= -f.lb) {
      /* End or Short-circuit */
      PFiltered(hasChanged, false, f)
    } else {
      val dom = doms(p)

      val fact = factors(p)

      val size = this.size(dom, fact)

      if (size <= -f.lb) {
        processUB(p + 1, f, factors, ps, hasChanged)
      } else {

        val thisBounds = f.lb - minTimes(dom, fact)

        val newDom = if (fact < 0) dom.removeUntil(Math.ceilDiv(thisBounds, -fact)) else dom.removeAfter(Math.floorDiv(thisBounds, -fact))
        LinearLe.shaves += 1

        doms(p) = newDom //val newDoms = doms.updated(i, newDom)
        val newF = Interval(thisBounds, f.ub - maxTimes(dom, fact)) + newDom.span * fact
        processUB(p + 1, newF, factors, ps, hasChanged + p)
      }

    }
  }
}