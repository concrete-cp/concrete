package concrete.heuristic.branch

import concrete._
import concrete.heuristic.Decision


final class IntervalBranch() extends BranchHeuristic {

  private val split = new Split()

  override def toString = "split"

  def compute(s: MAC, ps: ProblemState): ProblemState = ps

  override def branch(variable: Variable, domain: Domain, ps: ProblemState): Either[Outcome, (ProblemState, Decision, Decision)] = {
    if (domain.convex) {
      split.branch(variable, domain, ps)
    } else {
      var i = domain.head
      var j = domain.next(i)
      while (j == i + 1) {
        i = j
        j = domain.next(i)
      }
      Right(Split.splitAt(variable, domain, j, ps))
    }
  }

  def shouldRestart = false
}
