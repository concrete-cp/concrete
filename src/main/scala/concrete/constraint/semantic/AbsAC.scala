package concrete.constraint.semantic;

import concrete.Variable
import concrete.constraint.Constraint
import concrete.ProblemState
import concrete.Outcome
import concrete.Event

final class AbsAC(val result: Variable, val v0: Variable) extends Constraint(Array(result, v0)) {

  def init(ps: ProblemState) = ps

  //  val corresponding1 = result.dom.allValues map { v0.dom.index }
  //  val corresponding2 = result.dom.allValues map { v => v0.dom.index(-v) }
  //  val correspondingR = v0.dom.allValues map { v => result.dom.index(math.abs(v)) }

  def check(t: Array[Int]) = t(0) == math.abs(t(1))

  def revise(ps: ProblemState): Outcome = {
    val result = ps.dom(this.result)
    val x = ps.dom(v0)

    ps.filterDom(this.result) { v: Int => v >= 0 && (x.present(v) || x.present(-v)) }
      .filterDom(v0) { v: Int => result.present(math.abs(v)) }
      .entailIfFree(this)
  }

  override def toString(ps: ProblemState) =
    s"${result.toString(ps)} =AC= |${v0.toString(ps)}|";

  def advise(ps: ProblemState, event: Event, p: Int) = {
    val rSize = ps.card(result) * 3 / 2
    if (rSize > 200) { -1 }
    else {
      val eval = rSize * ps.card(v0)
      if (eval > 500) -1 else eval
    }

  }

  def simpleEvaluation = 1
}
