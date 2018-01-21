package concrete.heuristic.variable

import concrete._

class PropagationGuidedWDegOnDom(val pool: Seq[Variable]) extends ScoredVariableHeuristic
  with ConstraintWeighting with PropagationMeasure {

  def score(variable: Variable, dom: Domain, state: ProblemState): Double = {
    state.wDeg(variable) * (1.0 / dom.size - 1.0 / init(variable.id))
  }

  override def shouldRestart: Boolean = false

  override def compute(s: MAC, ps: ProblemState): ProblemState = {
    super[ConstraintWeighting].compute(s, super[PropagationMeasure].compute(s, ps))
  }
}
