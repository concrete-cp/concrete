package concrete.constraint.semantic;

import concrete.Domain
import concrete.Outcome
import concrete.ProblemState
import concrete.Variable
import concrete.constraint.Constraint
import concrete.Contradiction

/**
 * v0 - v1 != c
 *
 * or
 *
 * v0 != v1 + c
 */
final class Neq(v0: Variable, v1: Variable, c: Int = 0) extends Constraint(Array(v0, v1)) {
  def init(ps: ProblemState) = ps

  def check(t: Array[Int]) = (t(0) - t(1) != c)

  def revise(ps: ProblemState): Outcome = {
    val d0 = ps.dom(v0)
    val d1 = ps.dom(v1)
    ps
      .updateDom(v0, revise(d0, d1, c))
      .updateDom(v1, revise(d1, d0, -c))
      .entailIf(this, ch => ch.dom(v0) disjoint ch.dom(v1))
  }

  private def revise(variable: Domain, otherVar: Domain, c: Int): Domain = {
    if (otherVar.isAssigned)
      variable.remove(otherVar.singleValue + c)
    else
      variable
  }

  override def isConsistent(ps: ProblemState) = {
    val v0dom = ps.dom(v0)
    val r = !v0dom.isAssigned || {
      val v1dom = ps.dom(v1)
      !v1dom.isAssigned || (v0dom.head - v1dom.head != c)
    }

    if (r) ps else Contradiction
  }

  override def toString(ps: ProblemState) = s"${v0.toString(ps)} /= ${v1.toString(ps)}${
    if (c > 0) s" + $c"
    else if (c < 0) s" - ${-c}"
    else ""
  }"

  def advise(ps: ProblemState, p: Int) =
    if (ps.assigned(scope(p))) 2 else -1

  val simpleEvaluation = 2
}
