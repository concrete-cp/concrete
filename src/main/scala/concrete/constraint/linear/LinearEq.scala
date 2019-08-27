package concrete.constraint.linear

import bitvectors.BitVector
import com.typesafe.scalalogging.LazyLogging
import concrete._
import concrete.util.Interval

import scala.collection.immutable.IntMap

object LinearEq {
  def apply(constant: Int, factors: Array[Int], scope: Array[Variable]): LinearEq = {
    val (sf, ss, si) = Linear.sortIntervals(factors, scope)
    new LinearEq(constant, sf, ss, si)
  }

}

final class LinearEq(
                      constant: Int,
                      factors: Array[Int],
                      scope: Array[Variable],
                      val is: Array[Int])
  extends Linear(constant, factors, scope, SumMode.EQ)
    with IncrementalBoundPropagation
    with LazyLogging {

  import IncrementalBoundPropagation._

  private val negFactors = factors.map(-_)

  override def consistent(ps: ProblemState, mod: Iterable[Int]): Outcome = {
    val (f, vars, max) = updateF(ps, mod)
    clearMod()
    if (f.contains(0)) {
      ps.updateState(this, (ps, f, vars, max))
    } else {
      Contradiction(scope)
    }
  }

  @annotation.tailrec
  def altRevise(ps: ProblemState, doms: IntMap[Domain], f: Interval, vars: BitVector, neg: Boolean, looping: Boolean, currentMax: Int): Outcome = {
    if (currentMax <= -f.lb) {
      if (looping) {
        end(ps, doms, if (neg) -f else f, vars, currentMax)
      } else {
        altRevise(ps, doms, -f, vars, !neg, looping = true, currentMax)
      }
    } else {
      processUB(vars.nextSetBit(0), vars, doms, f, if (neg) negFactors else factors, ps) match {
        case PContradiction => Contradiction(scope)
        case PFiltered(newDoms, entailed, newF, newVars, max) =>
          require(newF != f || (newDoms eq doms))
          if ((newDoms eq doms) && looping) {
            end(ps, newDoms, if (neg) -f else f, newVars, math.min(currentMax, max))
          } else {
            altRevise(ps, newDoms, -newF, newVars, !neg, looping = true, math.min(currentMax, max))
          }
      }
    }

  }

  override def revise(ps: ProblemState, mod: BitVector): Outcome = {
    val (f, vars, max) = updateF(ps, mod)

    //if (bc) {
    proceed(ps, IntMap(), f, vars, max)
    //    } else {
    //      ps.updateState(this, (doms, f, vars, max))
    //    }
  }

  override def toString: String = toString("=BC=")

  override def toString(ps: ProblemState): String = {
    toString(ps, "=BC=") + " " + Option(ps(this)).map(s => ("previousState", s._2, s._3, s._4)).getOrElse("uninitialized")
  }

  def advise(ps: ProblemState, event: Event, pos: Int): Int = arity * 3

  def simpleEvaluation: Int = 3

  //def advise(ps: ProblemState, p: Int) = getEvaluation(ps)

  override def init(ps: ProblemState): ProblemState = initData(ps)

  protected def proceed(ps: ProblemState, doms: IntMap[Domain], f: Interval, vars: BitVector, max: Int): Outcome =
    altRevise(ps, doms, f, vars, neg = false, looping = false, max)

  private def end(ps: ProblemState, doms: IntMap[Domain], f: Interval, vars: BitVector, max: Int) = {
    val newPS = filter(doms, ps)

    newPS.updateState(this, (newPS, f, vars, max))
  }

}
