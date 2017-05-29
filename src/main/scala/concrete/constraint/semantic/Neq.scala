package concrete
package constraint
package semantic

/**
  * v0 - v1 != c
  *
  * or
  *
  * v0 != v1 + c
  */
final class Neq(v0: Variable, v1: Variable, c: Int = 0) extends Constraint(Array(v0, v1)) {
  val simpleEvaluation = 2

  def init(ps: ProblemState) = ps

  def check(t: Array[Int]) = (t(0) - t(1) != c)

  def revise(ps: ProblemState): Outcome = {
    revise(ps, v0, ps.dom(v1), c)
      .andThen { ps0: ProblemState =>
        revise(ps0, v1, ps0.dom(v0), -c)
      }
      .entailIf(this, ch => ch.dom(v0) disjoint ch.dom(v1).shift(c))
  }

  private def revise(ps: ProblemState, variable: Variable, otherVar: Domain, c: Int): Outcome = {
    if (otherVar.isAssigned) {
      ps.removeIfPresent(variable, otherVar.singleValue + c)
    } else {
      ps
    }
  }

  override def consistent(ps: ProblemState) = {
    val v0dom = ps.dom(v0)
    val r = !v0dom.isAssigned || {
      val v1dom = ps.dom(v1)
      !v1dom.isAssigned || (v0dom.head - v1dom.head != c)
    }
    if (r) ps else Contradiction(scope)
  }

  override def toString(ps: ProblemState) = s"${v0.toString(ps)} /= ${v1.toString(ps)}${
    if (c > 0) s" + $c"
    else if (c < 0) s" - ${-c}"
    else ""
  }"

  def advise(ps: ProblemState, event: Event, p: Int) = {
    // Entailment can be detected even if event is not an assigment
    //if (event <= Assignment) 2 else -1
    2
  }
}

class NeqC(x: Variable, y: Int) extends Constraint(x) {
  def advise(problemState: ProblemState, event: Event, pos: Int): Int = 2

  def init(ps: ProblemState) = ps

  def check(t: Array[Int]) = t(0) != y

  override def consistent(ps: ProblemState) = {
    val d = ps.dom(x)
    if (!d.isAssigned || d.singleValue != y) ps else Contradiction(scope)
  }

  def revise(ps: ProblemState): Outcome = {
    ps.removeIfPresent(x, y).entail(this)
  }

  def simpleEvaluation: Int = 1

  override def toString(ps: ProblemState) = s"${x.toString(ps)} /= $y"
}
