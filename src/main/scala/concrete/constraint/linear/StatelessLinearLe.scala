package concrete
package constraint
package linear

import bitvectors.BitVector
import com.typesafe.scalalogging.LazyLogging

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

  override def consistent(ps: ProblemState, mod: Traversable[Int]): Outcome = {
    if (updateF(ps)._1.lb <= 0) ps else Contradiction(scope)
  }

  override def revise(ps: ProblemState, mod: BitVector): Outcome = {
    val (f, max) = updateF(ps)
    if (f.ub <= 0) {
      ps.entail(this)
    } else if (max <= -f.lb) {
      ps
    } else {
      processUB(0, f, factors, ps) match {
        case PContradiction => Contradiction(scope)
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

  override def toString: String = toString("<=BC")

  override def toString(ps: ProblemState): String = toString(ps, "<=BC")

  def advise(ps: ProblemState, event: Event, p: Int): Int = if (event <= BoundRemoval) arity * 2 else -1

  def simpleEvaluation: Int = 3

  override def init(ps: ProblemState): Outcome = ps
}