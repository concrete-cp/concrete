package concrete
package heuristic.value

import concrete.heuristic.Branch

final class IntervalBranch(pm: ParameterManager) extends BranchHeuristic {

  private val split = new Split(pm)

  override def toString = "split"

  def compute(s: MAC, ps: ProblemState): ProblemState = ps

  override def branch(variable: Variable, domain: Domain, ps: ProblemState): Branch = {
    if (domain.convex) {
      split.branch(variable, domain, ps)
    } else {
      var i = domain.head
      var j = domain.next(i)
      while (j == i + 1) {
        i = j
        j = domain.next(i)
      }
      Split.splitAt(variable, domain, j, ps)
    }
  }

  def shouldRestart = false
}
