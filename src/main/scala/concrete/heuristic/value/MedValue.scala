package concrete
package heuristic
package value

import cspom.StatisticsManager

final class MedValue() extends ValueSelector {
  override def toString = "median"

  def compute(s: MAC, ps: ProblemState): ProblemState = ps

  override def select(ps: ProblemState, variable: Variable, candidates: Domain): (Outcome, Domain) = {
    (ps, Singleton(StatisticsManager.median(candidates.toSeq)))
  }

  def shouldRestart = false
}
