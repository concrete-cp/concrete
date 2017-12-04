package concrete.constraint.semantic

import concrete.constraint.{BC, Constraint}
import concrete.util.Interval
import concrete.{Contradiction, Outcome, ProblemState, Variable}

final class AbsDiffBC(val result: Variable, val v0: Variable, val v1: Variable)
  extends Constraint(Array(result, v0, v1)) with BC {

  def init(ps: ProblemState): ProblemState = ps

  def check(t: Array[Int]): Boolean = t(0) == math.abs(t(1) - t(2))

  override def shave(ps: ProblemState): Outcome = {
    val v1span = ps.span(v1)
    val v0span = ps.span(v0)

    ps.shaveDom(result, (v0span - v1span).abs)
      .andThen { ps =>

        val rspan = ps.span(result)

        Interval.union(v0span.intersect(v1span - rspan), v0span.intersect(v1span + rspan))
          .map { v0span =>
            ps.shaveDom(v0, v0span).andThen { ps =>
              Interval.union(
                v1span.intersect(v0span - rspan),
                v1span.intersect(v0span + rspan))
                .map(g => ps.shaveDom(v1, g))
                .getOrElse(Contradiction(Seq(v1)))
            }

          }
          .getOrElse(Contradiction(Seq(v0)))
      }

  }

  override def toString(ps: ProblemState): String =
    s"${result.toString(ps)} =BC= |${v0.toString(ps)} - ${v1.toString(ps)}|";

  def advise(ps: ProblemState, pos: Int): Int = 5

  def simpleEvaluation = 2
}
