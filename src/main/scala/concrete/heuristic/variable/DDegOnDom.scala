package concrete
package heuristic
package variable

class DDegOnDom(val pool: Seq[Variable], tieBreaker: VariableHeuristic)
  extends ScoredVariableHeuristic(tieBreaker) {

  def score(variable: Variable, dom: Domain, state: ProblemState) =
    variable.getDDegEntailed(state).toDouble / dom.size

  override def toString = s"max-ddeg/dom then $tieBreaker"

  override def shouldRestart: Boolean = tieBreaker.shouldRestart

}
