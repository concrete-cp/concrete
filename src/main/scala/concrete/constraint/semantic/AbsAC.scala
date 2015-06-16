package concrete.constraint.semantic;

import concrete.Domain
import concrete.Variable
import concrete.constraint.BCCompanion
import concrete.constraint.Constraint
import concrete.ProblemState
import concrete.Outcome

final class AbsAC(val result: Variable, val v0: Variable) extends Constraint(Array(result, v0))
  with BCCompanion {

  def skipIntervals = true

  //  val corresponding1 = result.dom.allValues map { v0.dom.index }
  //  val corresponding2 = result.dom.allValues map { v => v0.dom.index(-v) }
  //  val correspondingR = v0.dom.allValues map { v => result.dom.index(math.abs(v)) }

  def check(t: Array[Int]) = t(0) == math.abs(t(1))

  def revise(ps: ProblemState): Outcome = {
    val result = ps.dom(this.result)
    val x = ps.dom(v0)

    ps.filterDom(this.result) { v: Int => x.present(v) || x.present(-v) }
      .filterDom(v0) { v: Int => result.present(math.abs(v)) }
      .entailIfFree(this)
  }

  override def toString(ps: ProblemState) =
    s"${result.toString(ps)} =AC= |${v0.toString(ps)}|";

  def advise(ps: ProblemState, p: Int) = if (skip(ps)) -1 else ps.dom(result).size * 3 / 2 + ps.dom(v0).size

  def simpleEvaluation = 1
}
