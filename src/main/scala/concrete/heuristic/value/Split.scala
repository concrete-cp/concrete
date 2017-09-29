package concrete
package heuristic
package value

object Split {
  def splitAt(variable: Variable, dom: Domain, med:Int, ps: ProblemState): Branch = {

    splitAt(variable, med, dom.removeFrom(med), dom.removeUntil(med), ps)
  }

  def revSplitAt(variable: Variable, dom: Domain, med:Int, ps: ProblemState): Branch = {
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

  override def toString = "split"

  def compute(s: MAC, ps: ProblemState): ProblemState = ps

  override def branch(variable: Variable, domain: Domain, ps: ProblemState): Branch = {
    Split.splitAt(variable, domain,domain.median, ps)
  }

  def shouldRestart = false
}
