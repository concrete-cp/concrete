package concrete.constraint.linear

import bitvectors.BitVector
import com.typesafe.scalalogging.LazyLogging
import concrete._
import concrete.util.Interval
import cspom.Statistic

import scala.collection.immutable.IntMap

object LinearLe {
  @Statistic
  var shaves: Long = 0L

  def apply(constant: Int, factors: Array[Int], scope: Array[Variable], strict: Boolean, pm: ParameterManager): LinearLe = {
    val actualConstant = if (strict) constant - 1 else constant

    val (sf, ss, si) = Linear.sortIntervals(factors, scope)

    new LinearLe(actualConstant, sf, ss, si)
  }

}

final class LinearLe(
                      constant: Int,
                      factors: Array[Int],
                      scope: Array[Variable],
                      val is: Array[Int]) extends Linear(constant, factors, scope, SumMode.LE)
  with IncrementalBoundPropagation with LazyLogging {

  import IncrementalBoundPropagation._

  override def consistent(ps: ProblemState, mod: Iterable[Int]): Outcome = {
    val (f, vars, max) = updateF(ps, mod)
    clearMod()
    if (f.lb <= 0) {
      ps.updateState(this, (ps, f, vars, max))
    } else {
      Contradiction(scope)
    }
  }

  override def revise(ps: ProblemState, mod: BitVector): Outcome = {

    val (f, vars, max) = updateF(ps, mod)
    //if (bc) {
    val s = proceed(ps, IntMap(), f, vars, max)

    assert {
      s.andThen { s =>
        assert(f.ub > 0 || scope.forall(s.dom(_).isAssigned) || s.entailed.hasInactiveVar(this) || s.entailed.entailedReif(this), s"entailment was not correctly marked for ${toString(s)}")
        assert(scope.forall(s.dom(_).isAssigned) || s.entailed.hasInactiveVar(this) || s.entailed.entailedReif(this) || (0 until arity).forall(i => s.dom(scope(i)) == s(this)._1.dom(scope(i))),
          s"doms were not updated correctly: ${toString(ps)} -> ${toString(s)} with $mod")
        s
      }
      true
    }

    s
    //    } else {
    //      ps.updateState(this, (doms, f, vars, max))
    //    }

  }

  protected def proceed(ps: ProblemState, doms: IntMap[Domain], f: Interval, vars: BitVector, max: Int): Outcome = {
    if (f.ub <= 0) {
      ps.entail(this)
    } else if (max <= -f.lb) {
      ps.updateState(this, (ps, f, vars, max))
    } else {
      processUB(vars.nextSetBit(0), vars, doms, f, factors, ps) match {
        case PContradiction => Contradiction(scope)
        case PFiltered(newDoms, entailed, newF, newVars, newMax) =>
          val out = filter(newDoms, ps)
          if (entailed) {
            out.entail(this)
          } else {
            out.updateState(this, (out, newF, newVars, newMax))
          }
      }

    }
  }

  override def toString(ps: ProblemState): String = {
    val (lastPS, f, vars, maxI) = ps(this)
    toString(ps, "<=BC") + " with " + ((f, vars, maxI))
  }

  override def toString: String = toString("<=BC")

  def advise(ps: ProblemState, event: Event, pos: Int): Int = arity * 2

  def simpleEvaluation: Int = 3

  override def init(ps: ProblemState): ProblemState = initData(ps)

}
