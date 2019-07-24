package concrete
package constraint
package linear

import Linear.maxTimes
import Linear.minTimes
import bitvectors.BitVector
import concrete.util.Interval

import scala.collection.immutable.IntMap

object IncrementalBoundPropagation {
  sealed trait POutcome
  case object PContradiction extends POutcome
  case class PFiltered(updDomains: IntMap[Domain], entailed: Boolean, f: Interval, vars: BitVector, max: Int) extends POutcome
}

trait IncrementalBoundPropagation extends Linear with StatefulConstraint[(ProblemState, Interval, BitVector, Int)] {
  import IncrementalBoundPropagation._

  def is: Array[Int]

  protected def updateF(ps: ProblemState, mod: Iterable[Int]): (Interval, BitVector, Int) = {
    /*
     *  TODO: reinvestigate the detection of modified bounds (was disabled for reification but may still be interesting)
     */
    val (lastPS, f, vars, maxI) = ps(this)

    var newF = f
    var newVars = vars

    for (p <- mod) {
      val nd = ps.dom(scope(p))

      val oldspan = lastPS.span(scope(p))
      val newspan = nd.span

      val factor = factors(p)

      newF = newF.shrink(oldspan * factor) + (newspan * factor)

      if (nd.isAssigned) {
        newVars -= p
      }
    }

    (newF, newVars, maxI)

  }

  def initData(ps: ProblemState): ProblemState = {
    val doms = ps.doms(scope)
    val f = (doms lazyZip factors).map(_.span * _).reduce(_ + _) - constant
    val maxI = size(doms.head, factors.head)
    val vars = BitVector.filled(arity).filter(p => !doms(p).isAssigned)

    //proceed(ps, doms, f, vars, maxI)
    // println(s"$mode, arity $arity, $f")
    ps.updateState(this, (ps, f, vars, maxI))
  }

  protected def proceed(ps: ProblemState, doms: IntMap[Domain], f: Interval, vars: BitVector, max: Int): Outcome

  @annotation.tailrec
  final def processUB(
    i: Int,
    vars: BitVector,
    doms: IntMap[Domain],
    f: Interval,
    factors: Array[Int],
    ps: ProblemState,
    max: Int = 0): POutcome = {

    if (f.ub <= 0) {
      /* Entailed */
      val rm = if (i < 0) max else math.max(max, is(i))
      //require(i < 0 || rm <= is(i))
      PFiltered(doms, entailed = true, f, vars, rm)
      //if (i < 0) max else math.max(max, is(i)))
    } else if (f.lb > 0) {
      PContradiction
    } else if (i < 0) {
      PFiltered(doms, entailed = false, f, vars, max)
    } else if (is(i) <= -f.lb) {
      /* Short-circuit */
      val rm = math.max(max, is(i))
      //require(rm <= is(i), (max, rm, doms.toSeq, factors.toSeq, is.toSeq))
      PFiltered(doms, entailed = false, f, vars, rm)
    } else {
      val dom = doms.getOrElse(i, ps.dom(scope(i)))

      val fact = factors(i)

      val size = this.size(dom, fact)

      if (size <= -f.lb) {
        processUB(vars.nextSetBit(i + 1), vars, doms, f,
          factors, ps, math.max(max, size))
      } else {

        val thisBounds = f.lb - minTimes(dom, fact)

        val newDom = if (fact < 0) dom.removeUntil(util.Math.ceilDiv(thisBounds, -fact)) else dom.removeAfter(Math.floorDiv(thisBounds, -fact))
        LinearLe.shaves += 1

        val newMax = math.max(max, this.size(newDom, fact))

        val newDoms = doms.updated(i, newDom) //val newDoms = doms.updated(i, newDom)
        val newF = Interval(thisBounds, f.ub - maxTimes(dom, fact)) + newDom.span * fact
        if (newDom.isAssigned) {
          val newVars = vars - i
          processUB(newVars.nextSetBit(i + 1), newVars, newDoms, newF, factors, ps, newMax)
        } else {
          processUB(vars.nextSetBit(i + 1), vars, newDoms, newF, factors, ps, newMax)
        }
      }

    }
  }

}
