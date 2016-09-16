package concrete
package heuristic

final class RevSplit(pm: ParameterManager) extends BranchHeuristic {

  override def toString = "rev-split";

  def compute(p: Problem) {
    // Nothing to compute
  }

  override def branch(variable: Variable, domain: Domain, ps: ProblemState) = {
    Split.revSplitAt(variable, domain.median, ps)
  }

  def shouldRestart = false
}
