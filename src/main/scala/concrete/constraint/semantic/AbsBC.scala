package concrete.constraint.semantic;

import concrete.constraint.Constraint
import concrete.Variable
import concrete.constraint.BC
import concrete.Domain
import concrete.ProblemState
import concrete.Outcome
import concrete.util.Interval
import concrete.Contradiction

final class AbsBC(val result: Variable, val v0: Variable) extends Constraint(Array(result, v0)) with BC {
  //  val corresponding1 = result.dom.allValues map { v0.dom.index }
  //  val corresponding2 = result.dom.allValues map { v => v0.dom.index(-v) }
  //  val correspondingR = v0.dom.allValues map { v => result.dom.index(math.abs(v)) }

  def check(t: Array[Int]) = t(0) == math.abs(t(1))

  def shave(ps: ProblemState): Outcome = {

    ps.shaveDom(result, ps.span(v0).abs)
      .andThen { ps =>

        val ri = ps.span(result)
        val v0span = ps.span(v0)

        Interval.union(v0span intersect ri, v0span intersect -ri)
          .map {
            nv0 => ps.shaveDom(this.v0, nv0)
          }
          .getOrElse {
            Contradiction
          }

      }
  }

  override def toString(ps: ProblemState) = s"${result.toString(ps)} =BC= |${v0.toString(ps)}|";

  def advise(ps: ProblemState, p: Int) = 6

  def simpleEvaluation = 1
}
