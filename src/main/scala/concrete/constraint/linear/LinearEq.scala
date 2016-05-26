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

object LinearEq {
  def apply(constant: Int, factors: Array[Int], scope: Array[Variable]) = {
    val (sf, ss, si) = Linear.sortIntervals(factors, scope)
    new LinearEq(constant, sf, ss, si)
  }

}

final class LinearEq(
  constant: Int,
  factors: Array[Int],
  scope: Array[Variable],
  val is: Array[Int])
    extends Linear(constant, factors, scope, SumEQ)
    with IncrementalBoundPropagation
    with Removals
    with LazyLogging {

  import IncrementalBoundPropagation._

  private val negFactors = factors.map(-_)

  override def isConsistent(ps: ProblemState, mod: BitVector) = {
    val (doms, f, vars, max, bc) = updateF(ps, mod)
    clearMod()
    if (!bc || f.contains(0)) {
      ps.updateState(this, (doms, f, vars, max))
    } else {
      Contradiction
    }
  }

  @annotation.tailrec
  def altRevise(ps: ProblemState, doms: Array[Domain], changed: BitVector, f: Interval, vars: BitVector, neg: Boolean, looping: Boolean, currentMax: Int): Outcome = {
    if (currentMax <= -f.lb) {
      if (looping) {
        end(ps, doms, changed, if (neg) -f else f, vars, currentMax)
      } else {
        altRevise(ps, doms, changed, -f, vars, !neg, true, currentMax)
      }
    } else {
      processUB(vars.nextSetBit(0), vars, doms, f, if (neg) negFactors else factors, ps) match {
        case PContradiction => Contradiction
        case PFiltered(newChanged, entailed, newF, newVars, max) =>
          require(newF != f || newChanged.isEmpty)
          if (newChanged.isEmpty && looping) {
            end(ps, doms, changed, if (neg) -f else f, newVars, math.min(currentMax, max))
          } else {
            altRevise(ps, doms, newChanged | changed, -newF, newVars, !neg, true, math.min(currentMax, max))
          }
      }
    }

  }

  override def revise(ps: ProblemState, mod: BitVector): Outcome = {
    val (doms, f, vars, max, bc) = updateF(ps, mod)

    if (bc) {
      proceed(ps, doms, f, vars, max)
    } else {
      ps.updateState(this, (doms, f, vars, max))
    }
  }

  def proceed(ps: ProblemState, doms: Array[Domain], f: Interval, vars: BitVector, max: Int) =
    altRevise(ps, doms, BitVector.empty, f, vars, false, false, max)

  def end(ps: ProblemState, doms: Array[Domain], changed: BitVector, f: Interval, vars: BitVector, max: Int) = {
    filter(changed, doms, ps)
      .updateState(this, (doms, f, vars, max))
  }

  override def toString() = toString("=BC=")

  override def toString(ps: ProblemState) = {
    toString(ps, "=BC=") + " " + Option(ps(this))
      .map {
        case (doms, f, vars, maxI) => (doms.toSeq, f, vars, maxI).toString
      }
      .getOrElse("uninitialized")
  }

  //def advise(ps: ProblemState, p: Int) = getEvaluation(ps)

  def getEvaluation(ps: ProblemState) = arity * 3

  def simpleEvaluation: Int = 3

  override def init(ps: ProblemState) = initData(ps)

  override def entailable = false

}
