package concrete
package heuristic

object Split {
  def splitAt(variable: Variable, med: Int, ps: ProblemState): Branch = {
    val dom = ps.dom(variable)

    splitAt(variable, med, dom.removeFrom(med), dom.removeUntil(med), ps)
  }

  def revSplitAt(variable: Variable, med: Int, ps: ProblemState): Branch = {
    val dom = ps.dom(variable)

    splitAt(variable, med, dom.removeUntil(med), dom.removeFrom(med), ps)
  }

  def splitAt(variable: Variable, med: Int, b1: Domain, b2: Domain, ps: ProblemState): Branch = {
    new Branch(
      ps.updateDomNonEmptyNoCheck(variable, b1), Seq((variable, BoundRemoval(b1))),
      ps.updateDomNonEmptyNoCheck(variable, b2), Seq((variable, BoundRemoval(b2))),
      s"${variable.toString(ps)} < $med", s"${variable.toString(ps)} >= $med")
  }
}

final class Split(pm: ParameterManager) extends BranchHeuristic {

  override def toString = "split";

  def compute(p: Problem) {
    // Nothing to compute
  }

  override def branch(variable: Variable, domain: Domain, ps: ProblemState) = {
    Split.splitAt(variable, domain.median, ps)
  }

  def shouldRestart = false
}
