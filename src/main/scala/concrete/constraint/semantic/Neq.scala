package concrete.constraint.semantic;

import concrete.Domain
import concrete.Outcome
import concrete.ProblemState
import concrete.Variable
import concrete.constraint.Constraint

/**
 * v0 - v1 != c
 *
 * or
 *
 * v0 != v1 + c
 */
final class Neq(v0: Variable, v1: Variable, c: Int = 0) extends Constraint(Array(v0, v1)) {

  def check(t: Array[Int]) = (t(0) - t(1) != c)

  def revise(ps: ProblemState): Outcome = {
    val d0 = ps.dom(v0)
    val d1 = ps.dom(v1)
    ps
      .updateDom(v0, revise(d0, d1, c))
      .updateDom(v1, revise(d1, d0, -c))
      .andThen { ch =>
        if (ch.dom(v0) disjoint ch.dom(v1)) {
          ch.entail(this)
        } else {
          ch
        }
      }

  }

  private def revise(variable: Domain, otherVar: Domain, c: Int): Domain = {
    if (otherVar.size == 1) variable.remove(otherVar.head + c) else variable
  }

  override def isConsistent(ps: ProblemState) = {
    ps.dom(v0).size > 1 || ps.dom(v1).size > 1 || ps.dom(v0).head != ps.dom(v1).head
  }

  override def toString(ps: ProblemState) = s"${v0.toString(ps)} /= ${v1.toString(ps)}${
    if (c > 0) s" + $c"
    else if (c < 0) s" - ${-c}"
    else ""
  }"

  def advise(ps: ProblemState, p: Int) = if (ps.dom(scope(p)).size > 1) -1 else 2

  val simpleEvaluation = 2
}
