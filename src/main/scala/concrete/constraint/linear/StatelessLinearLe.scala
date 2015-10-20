package concrete.constraint.linear

import concrete.ParameterManager
import com.typesafe.scalalogging.LazyLogging
import concrete.constraint.Removals
import concrete.Contradiction
import concrete.Variable
import concrete.ProblemState
import cspom.Statistic
import concrete.Outcome
import concrete.Domain
import concrete.util.Interval
import cspom.util.BitVector
import concrete.util.Math

object StatelessLinearLe {
  def apply(constant: Int, factors: Array[Int], scope: Array[Variable], strict: Boolean, pm: ParameterManager) = {
    val actualConstant = if (strict) constant - 1 else constant

    val (sf, ss, si) = Linear.sortIntervals(factors, scope)

    new StatelessLinearLe(actualConstant, sf, ss, si)
  }

}

final class StatelessLinearLe(
  constant: Int,
  factors: Array[Int],
  scope: Array[Variable],
  val is: Array[Int]) extends Linear(constant, factors, scope, SumLE) with StatelessBoundPropagation
    with LazyLogging {

  import StatelessBoundPropagation._

  override def isConsistent(ps: ProblemState) = {
    val f = updateF(ps)._1
    if (f.lb <= 0) {
      ps
    } else {
      Contradiction
    }
  }

  override def revise(ps: ProblemState): Outcome = {
    val (f, max) = updateF(ps)
    if (f.ub <= 0) {
      ps.entail(this)
    } else if (max <= -f.lb) {
      ps
    } else {
      processUB(0, f, factors, ps) match {
        case PContradiction => Contradiction
        case PFiltered(changed, entailed, newF) =>
          val out = filter(changed, doms, ps)
          if (entailed) {
            out.entail(this)
          } else {
            out
          }
      }

    }

  }

  override def toString() = toString("<=BC")

  override def toString(ps: ProblemState) = toString(ps, "<=BC")

  def advise(ps: ProblemState, p: Int) = arity * 2

  def simpleEvaluation: Int = 3

  override def init(ps: ProblemState) = ps
}