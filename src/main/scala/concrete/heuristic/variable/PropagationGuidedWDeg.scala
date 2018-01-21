package concrete.heuristic.variable

import concrete._

class PropagationGuidedWDeg(val pool: Seq[Variable]) extends ScoredVariableHeuristic with PropagationMeasure
  with ConstraintWeighting {

  def score(variable: Variable, dom: Domain, state: ProblemState): Double = {
    state.wDeg(variable) * (1 - dom.size.toDouble / init(variable.id))
  }

  override def shouldRestart: Boolean = false
}
