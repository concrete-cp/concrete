package concrete.heuristic;

import concrete.Variable
import concrete.Problem
import concrete.Domain
import cspom.StatisticsManager
import concrete.ProblemState
import concrete.ParameterManager

final class IntervalBranch(pm: ParameterManager) extends BranchHeuristic {

  override def toString = "split";

  def compute(p: Problem) {
    // Nothing to compute
  }

  private val split = new Split(pm)

  override def branch(variable: Variable, domain: Domain, ps: ProblemState) = {
    if (domain.convex) {
      split.branch(variable, domain, ps)
    } else {
      var i = domain.head
      var j = domain.next(i)
      while (j == i + 1) {
        i = j
        j = domain.next(i)
      }
      Split.splitAt(variable, j, ps)
    }
  }

  def shouldRestart = false
}
