package concrete
package heuristic
package value

final class RevSplit(pm: ParameterManager) extends BranchHeuristic {

  override def toString = "rev-split"

  def compute(s: MAC, ps: ProblemState): ProblemState = ps

  override def branch(variable: Variable, domain: Domain, ps: ProblemState): Branch = {
    Split.revSplitAt(variable, domain, domain.median, ps)
  }

  def shouldRestart = false
}
