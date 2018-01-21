package concrete
package heuristic
package variable

class DDegOnDom(val pool: Seq[Variable]) extends ScoredVariableHeuristic {

  override def compute(s: MAC, ps: ProblemState): ProblemState = ps

  def score(variable: Variable, dom: Domain, state: ProblemState): Double =
    variable.getDDegEntailed(state).toDouble / dom.size

  override def toString = s"max-ddeg/dom"

  override def shouldRestart: Boolean = false

}
