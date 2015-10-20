package concrete.constraint.linear

import com.typesafe.scalalogging.LazyLogging
import concrete.constraint.Removals
import concrete.Contradiction
import concrete.Variable
import concrete.ProblemState
import cspom.util.BitVector
import concrete.Outcome
import concrete.Domain
import concrete.util.Interval

object StatelessLinearEq {
  def apply(constant: Int, factors: Array[Int], scope: Array[Variable]) = {
    val (sf, ss, si) = Linear.sortIntervals(factors, scope)
    new StatelessLinearEq(constant, sf, ss, si)
  }
}

final class StatelessLinearEq(
  constant: Int,
  factors: Array[Int],
  scope: Array[Variable],
  val is: Array[Int])
    extends Linear(constant, factors, scope, SumEQ)
    with StatelessBoundPropagation
    with LazyLogging {

  import StatelessBoundPropagation._

  private val negFactors = factors.map(-_)

  override def isConsistent(ps: ProblemState) = {
    val f = updateF(ps)._1

    if (f.contains(0)) {
      ps
    } else {
      Contradiction
    }
  }

  @annotation.tailrec
  def altRevise(ps: ProblemState, changed: BitVector, f: Interval, neg: Boolean, looping: Boolean, max: Int): Outcome = {
    if (max <= -f.lb) {
      if (looping) {
        filter(changed, doms, ps)
      } else {
        altRevise(ps, changed, -f, !neg, true, max)
      }
    } else {
      processUB(0, f, if (neg) negFactors else factors, ps) match {
        case PContradiction => Contradiction
        case PFiltered(newChanged, entailed, newF) =>
          require(newF != f || newChanged.isEmpty)
          if (newChanged.isEmpty && looping) {
            filter(changed, doms, ps)
          } else {
            altRevise(ps, newChanged | changed, -newF, !neg, true, max)
          }
      }
    }

  }

  override def revise(ps: ProblemState): Outcome = {
    val (f, max) = updateF(ps)

    altRevise(ps, BitVector.empty, f, false, false, max)
  }

  override def toString() = toString("=BC=")

  override def toString(ps: ProblemState) = {
    toString(ps, "=BC=")
  }

  //def advise(ps: ProblemState, p: Int) = getEvaluation(ps)

  def advise(ps: ProblemState, p: Int) = arity * 3

  def simpleEvaluation: Int = 3

  override def init(ps: ProblemState) = ps

  override def entailable = false

}
