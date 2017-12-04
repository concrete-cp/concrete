package concrete
package heuristic
package variable

final class DDeg(val pool: Seq[Variable], tieBreaker: VariableHeuristic)
  extends ScoredVariableHeuristic(tieBreaker) {

  def score(variable: Variable, dom: Domain, state: ProblemState): Double =
    variable.getDDegEntailed(state).toDouble

  override def toString = s"max-ddeg then $tieBreaker"

  override def shouldRestart: Boolean = tieBreaker.shouldRestart

}
