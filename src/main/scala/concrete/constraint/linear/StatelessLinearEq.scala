package concrete
package constraint
package linear

import bitvectors.BitVector
import com.typesafe.scalalogging.LazyLogging
import concrete.util.Interval

import scala.collection.immutable.IntMap

object StatelessLinearEq {
  def apply(constant: Int, factors: Array[Int], scope: Array[Variable]): StatelessLinearEq = {
    val (sf, ss, si) = Linear.sortIntervals(factors, scope)
    new StatelessLinearEq(constant, sf, ss, si)
  }
}

final class StatelessLinearEq(
                               constant: Int,
                               factors: Array[Int],
                               scope: Array[Variable],
                               val is: Array[Int])
  extends Linear(constant, factors, scope, SumMode.EQ)
    with StatelessBoundPropagation
    with LazyLogging {

  import StatelessBoundPropagation._

  private val negFactors = factors.map(-_)

  override def consistent(ps: ProblemState, mod: Iterable[Int]): Outcome = {
    if (updateF(ps)._1.contains(0)) ps else Contradiction(scope)
  }

  @annotation.tailrec
  def altRevise(ps: ProblemState, doms: IntMap[Domain], f: Interval, neg: Boolean, looping: Boolean, max: Int): Outcome = {
    if (max <= -f.lb) {
      if (looping) {
        filter(doms, ps)
      } else {
        altRevise(ps, doms, -f, !neg, looping = true, max)
      }
    } else {
      processUB(doms, 0, f, if (neg) negFactors else factors, ps) match {
        case PContradiction => Contradiction(scope)
        case PFiltered(newDoms, _, newF) =>
          require(newF != f || (newDoms eq doms), s"$newF, $f, ${ps.doms(scope).toSeq}, $doms, $newDoms")
          if ((newDoms eq doms) && looping) {
            filter(newDoms, ps)
          } else {
            altRevise(ps, newDoms, -newF, !neg, looping = true, max)
          }
      }
    }

  }

  override def revise(ps: ProblemState, mod: BitVector): Outcome = {
    val (f, max) = updateF(ps)

    altRevise(ps, IntMap(), f, neg = false, looping = false, max)
  }

  override def toString: String = toString("=BC=")

  override def toString(ps: ProblemState): String = {
    toString(ps, "=BC=")
  }

  //def advise(ps: ProblemState, p: Int) = getEvaluation(ps)

  def advise(ps: ProblemState, event: Event, p: Int): Int = if (event <= BoundRemoval) arity * 3 else -1

  def simpleEvaluation: Int = 3

  override def init(ps: ProblemState): ProblemState = ps

}
