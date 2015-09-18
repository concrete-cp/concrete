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
  val initBound = Interval(0, 0)

  def apply(constant: Int, factors: Array[Int], scope: Array[Variable]) = {
    val (sf, ss, si) = Linear.sortIntervals(factors, scope)
    new LinearEq(constant, sf, ss, si)
  }

}

//final class LinearEq(
//  constant: Int,
//  factors: Array[Int],
//  scope: Array[Variable],
//  val is: Array[Int])
//    extends Linear(constant, factors, scope, SumEQ)
//    with IncrementalBoundPropagation
//    with Removals
//    with LazyLogging {
//
//  import IncrementalBoundPropagation._
//
//  private val negFactors = factors.map(-_)
//
//  //  override def isConsistent(ps: ProblemState) = {
//  //    val (doms, f, vars) = updateF(ps)
//  //    if (f.contains(0)) {
//  //      ps.updateState(this, (doms, f, vars))
//  //    } else {
//  //      Contradiction
//  //    }
//  //  }
//
//  @annotation.tailrec
//  def altRevise(ps: ProblemState, doms: Array[Domain], changed: BitVector, f: Interval, vars: BitVector, e: Boolean, neg: Boolean, looping: Boolean, max: Int): Outcome = {
//    processUB(vars.nextSetBit(0), vars, doms, f, if (neg) negFactors else factors, ps) match {
//      case PContradiction => Contradiction
//      case PFiltered(newChanged, entailed, newF, newVars) =>
//        assert(!e || entailed)
//        if (e || (newF == f && looping)) {
//          end(ps, doms, newChanged | changed, if (neg) -f else f, newVars, e, max)
//        } else {
//          altRevise(ps, doms, newChanged | changed, -newF, newVars, entailed, !neg, true, max)
//        }
//    }
//  }
//
//  override def revise(ps: ProblemState, mod: Seq[Int]): Outcome = {
//    val (doms, f, vars, max) = updateF(ps, mod)
//
//    //reviseUB(ps, doms, BitVector.empty, f, vars, false, true)
//    altRevise(ps, doms, BitVector.empty, f, vars, false, false, false, max)
//
//  }
//
//  def end(ps: ProblemState, doms: Array[Domain], changed: BitVector, f: Interval, vars: BitVector, e: Boolean, max: Int) = {
//    //println(Thread.currentThread().getStackTrace.size)
//    val out = filter(changed, doms, ps)
//    if (e) {
//      out.entail(this)
//    } else {
//      out.updateState(this, (doms, f, vars, max))
//    }
//  }
//
//  override def toString() = toString("=BC=")
//
//  override def toString(ps: ProblemState) = toString(ps, "=BC=") + ps(this)
//
//  //def advise(ps: ProblemState, p: Int) = getEvaluation(ps)
//
//  def getEvaluation(ps: ProblemState) = arity * 3
//
//  def simpleEvaluation: Int = 3
//
//  override def init(ps: ProblemState) = initData(ps)
//
//}

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

  //  override def isConsistent(ps: ProblemState) = {
  //    val (doms, f, vars) = updateF(ps)
  //    if (f.contains(0)) {
  //      ps.updateState(this, (doms, f, vars))
  //    } else {
  //      Contradiction
  //    }
  //  }

  @annotation.tailrec
  def altRevise(ps: ProblemState, doms: Array[Domain], changed: BitVector, f: Interval, vars: BitVector, e: Boolean, neg: Boolean, looping: Boolean, currentMax: Int): Outcome = {
    processUB(vars.nextSetBit(0), vars, doms, f, if (neg) negFactors else factors, ps) match {
      case PContradiction => Contradiction
      case PFiltered(newChanged, entailed, newF, newVars, max) =>
        assert(!e || entailed)
        if (e || (newF == f && looping)) {
          end(ps, doms, newChanged | changed, if (neg) -f else f, newVars, e, math.min(currentMax, max))
        } else {
          altRevise(ps, doms, newChanged | changed, -newF, newVars, entailed, !neg, true, math.min(currentMax, max))
        }
    }

  }

  override def revise(ps: ProblemState, mod: Seq[Int]): Outcome = {
    val (doms, f, vars, max) = updateF(ps, mod)

    if (max > -f.lb || max > f.ub) {
      altRevise(ps, doms, BitVector.empty, f, vars, false, false, false, max)
    } else {
      ps.updateState(this, (doms, f, vars, max))
    }

    //

    //reviseUB(ps, doms, BitVector.empty, f, vars, false, true)

  }

  def end(ps: ProblemState, doms: Array[Domain], changed: BitVector, f: Interval, vars: BitVector, e: Boolean, max: Int) = {
    //println(Thread.currentThread().getStackTrace.size)
    val out = filter(changed, doms, ps)
    if (e) {
      out.entail(this)
    } else {
      out.updateState(this, (doms, f, vars, max))
    }
  }

  override def toString() = toString("=BC=")

  override def toString(ps: ProblemState) = toString(ps, "=BC=") + ps(this)

  //def advise(ps: ProblemState, p: Int) = getEvaluation(ps)

  def getEvaluation(ps: ProblemState) = arity * 3

  def simpleEvaluation: Int = 3

  override def init(ps: ProblemState) = initData(ps)

}
