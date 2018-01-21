package concrete
package heuristic
package variable

final class DDeg(val pool: Seq[Variable])
  extends ScoredVariableHeuristic {

  override def compute(s: MAC, ps: ProblemState): ProblemState = ps

  def score(variable: Variable, dom: Domain, state: ProblemState): Double = variable.getDDegEntailed(state).toDouble

  override def toString = s"max-ddeg"

  override def shouldRestart: Boolean = false

}
