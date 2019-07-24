package concrete
package constraint
package linear

import concrete.constraint.linear.Linear.{maxTimes, minTimes}
import concrete.util.Interval

import scala.collection.immutable.IntMap


object StatelessBoundPropagation {

  sealed trait POutcome

  case class PFiltered(newDoms: IntMap[Domain], entailed: Boolean, f: Interval) extends POutcome

  case object PContradiction extends POutcome

}

trait StatelessBoundPropagation extends Linear {

  import StatelessBoundPropagation._

  protected val initBound: Interval = -Interval(constant, constant)

  @annotation.tailrec
  final def processUB(
                       doms: IntMap[Domain],
                       p: Int,
                       f: Interval,
                       factors: Array[Int],
                       ps: ProblemState): POutcome = {

    if (f.ub <= 0) {
      /* Entailed */
      PFiltered(doms, entailed = true, f)
    } else if (f.lb > 0) {
      PContradiction
    } else if (p >= arity || is(p) <= -f.lb) {
      /* End or Short-circuit */
      PFiltered(doms, entailed = false, f)
    } else {
      val dom = doms.getOrElse(p, ps.dom(scope(p)))

      val fact = factors(p)

      val size = this.size(dom, fact)

      if (size <= -f.lb) {
        processUB(doms, p + 1, f, factors, ps)
      } else {

        val thisBounds = f.lb - minTimes(dom, fact)

        val newDom = if (fact < 0) dom.removeUntil(util.Math.ceilDiv(thisBounds, -fact)) else dom.removeAfter(Math.floorDiv(thisBounds, -fact))
        LinearLe.shaves += 1

        val newDoms = doms.updated(p, newDom) //val newDoms = doms.updated(i, newDom)
        val newF = Interval(thisBounds, f.ub - maxTimes(dom, fact)) + newDom.span * fact
        processUB(newDoms, p + 1, newF, factors, ps)
      }

    }
  }

  protected def is: Array[Int]

  protected def updateF(ps: ProblemState): (Interval, Int) = {
    var f = initBound
    var p = arity - 1
    var maxI = 0
    while (p >= 0) {
      val dom = ps.dom(scope(p))

      val span = dom.span * factors(p)

      f += span

      maxI = math.max(maxI, span.ub - span.lb)

      p -= 1
    }

    (f, maxI)

  }
}