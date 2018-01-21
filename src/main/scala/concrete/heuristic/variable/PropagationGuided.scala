package concrete.heuristic.variable

import concrete._

class PropagationGuided(val pool: Seq[Variable]) extends ScoredVariableHeuristic with PropagationMeasure {

  def score(variable: Variable, dom: Domain, state: ProblemState): Double = {
    (init(variable.id) - dom.size).toDouble / init(variable.id)
  }

  override def shouldRestart: Boolean = false
}

trait PropagationMeasure extends VariableHeuristic {
  val init = new Array[Int](pool.map(_.id).max + 1)

  def compute(s: MAC, ps: ProblemState): ProblemState = {
    for (v <- pool) {
      init(v.id) = ps.card(v)
    }
    ps //super.compute(s, ps)
  }

}