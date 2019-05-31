package concrete
package constraint
package linear

import Linear.maxTimes
import Linear.minTimes
import bitvectors.BitVector
import concrete.util.Interval

object IncrementalBoundPropagation {
  sealed trait POutcome
  case object PContradiction extends POutcome
  case class PFiltered(changed: BitVector, entailed: Boolean, f: Interval, vars: BitVector, max: Int) extends POutcome
}

trait IncrementalBoundPropagation extends Linear with StatefulConstraint[(Array[Domain], Interval, BitVector, Int)] {
  import IncrementalBoundPropagation._

  def is: Array[Int]

  protected def updateF(ps: ProblemState, mod: Traversable[Int]): (Array[Domain], Interval, BitVector, Int) = {
    /*
     *  TODO: reinvestigate the detection of modified bounds (was disabled for reification but may still be interesting)
     */
    val (doms, f, vars, maxI) = ps(this)

    val newDoms = doms.clone

    var newF = f
    var newVars = vars

    for (p <- mod) {
      val nd = ps.dom(scope(p))

      newDoms(p) = nd

      val oldspan = doms(p).span
      val newspan = nd.span

      val factor = factors(p)

      newF = newF.shrink(oldspan * factor) + (newspan * factor)

      if (nd.size == 1) {
        newVars -= p
      }
    }

    (newDoms, newF, newVars, maxI)

  }

  def initData(ps: ProblemState): ProblemState = {
    val doms = ps.doms(scope)
    val f = (doms, factors).zipped.map(_.span * _).reduce(_ + _) - constant
    val maxI = size(doms.head, factors.head)
    val vars = BitVector.filled(arity).filter(p => !doms(p).isAssigned)

    //proceed(ps, doms, f, vars, maxI)
    // println(s"$mode, arity $arity, $f")
    ps.updateState(this, (doms, f, vars, maxI))
  }

  protected def proceed(ps: ProblemState, doms: Array[Domain], f: Interval, vars: BitVector, max: Int): Outcome

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
      PFiltered(hasChanged, entailed = true, f, vars, rm)
      //if (i < 0) max else math.max(max, is(i)))
    } else if (f.lb > 0) {
      PContradiction
    } else if (i < 0) {
      PFiltered(hasChanged, entailed = false, f, vars, max)
    } else if (is(i) <= -f.lb) {
      /* Short-circuit */
      val rm = math.max(max, is(i))
      //require(rm <= is(i), (max, rm, doms.toSeq, factors.toSeq, is.toSeq))
      PFiltered(hasChanged, entailed = false, f, vars, rm)
    } else {
      val dom = doms(i)

      val fact = factors(i)

      val size = this.size(dom, fact)

      if (size <= -f.lb) {
        processUB(vars.nextSetBit(i + 1), vars, doms, f,
          factors, ps, hasChanged, math.max(max, size))
      } else {

        val thisBounds = f.lb - minTimes(dom, fact)

        val newDom = if (fact < 0) dom.removeUntil(util.Math.ceilDiv(thisBounds, -fact)) else dom.removeAfter(Math.floorDiv(thisBounds, -fact))
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
