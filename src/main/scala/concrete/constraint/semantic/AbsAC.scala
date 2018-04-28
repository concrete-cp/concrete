package concrete.constraint.semantic

;

import bitvectors.BitVector
import concrete.constraint.Constraint
import concrete.{Event, Outcome, ProblemState, Variable}

final class AbsAC(val result: Variable, val v0: Variable, val skipLarge: Boolean = true) extends Constraint(Array(result, v0)) {

  def init(ps: ProblemState): ProblemState = ps

  //  val corresponding1 = result.dom.allValues map { v0.dom.index }
  //  val corresponding2 = result.dom.allValues map { v => v0.dom.index(-v) }
  //  val correspondingR = v0.dom.allValues map { v => result.dom.index(math.abs(v)) }

  def check(t: Array[Int]): Boolean = t(0) == math.abs(t(1))

  def revise(ps: ProblemState, mod: BitVector): Outcome = {
    val result = ps.dom(this.result)
    val x = ps.dom(v0)

    ps.filterDom(this.result) { v: Int => v >= 0 && (x(v) || x(-v)) }
      .filterDom(v0) { v: Int => result(math.abs(v)) }
      .entailIfFree(this)
  }

  override def toString(ps: ProblemState) =
    s"${result.toString(ps)} =AC= |${v0.toString(ps)}|"

  def advise(ps: ProblemState, event: Event, p: Int): Int = {
    val rSize = ps.card(result) * 3 / 2
    if (skipLarge && rSize > 200) {
      -2
    } else {
      val eval = rSize * ps.card(v0)
      if (skipLarge && eval > 1000) -2 else eval
    }

  }

  def simpleEvaluation = 1
}
