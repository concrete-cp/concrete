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

    ps.shaveDom(result, ps.dom(v0).span.abs)
      .andThen { ps =>

        val ri = ps.dom(result).span
        val v0span = ps.dom(v0).span

        Interval.union(v0span intersect ri, v0span intersect -ri).map {
          nv0 => ps.shaveDom(this.v0, nv0).entailIfFree(this)
        }
          .getOrElse {
            Contradiction
          }

      }
  }

  override def toString(domains: IndexedSeq[Domain], state: Unit) = domains(0) + " = |" + domains(1) + "|";

  def advise(domains: IndexedSeq[Domain], p: Int) = 6

  def simpleEvaluation = 1
}
