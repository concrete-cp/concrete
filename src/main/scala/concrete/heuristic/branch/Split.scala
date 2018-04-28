package concrete.heuristic.branch

import concrete._
import concrete.heuristic.{Decision, Reduce}

object Split {
  def splitAt(variable: Variable, dom: Domain, med: Int, ps: ProblemState): (ProblemState, Decision, Decision) = {
    splitAt(variable, med, dom.removeFrom(med), dom.removeUntil(med), ps)
  }

  def revSplitAt(variable: Variable, dom: Domain, med: Int, ps: ProblemState): (ProblemState, Decision, Decision) = {
    splitAt(variable, med, dom.removeUntil(med), dom.removeFrom(med), ps)
  }

  def splitAt(variable: Variable, med: Int, b1: Domain, b2: Domain, ps: ProblemState): (ProblemState, Decision, Decision) = (
    ps,
    Reduce(variable, b1),
    Reduce(variable, b2)
//    new Branch(
//      ps.updateDomNonEmptyNoCheck(variable, b1), Seq((variable, BoundRemoval(b1))), Unavailable, s"${variable.toString(ps)} < $med"),
//    new Branch(
//      ps.updateDomNonEmptyNoCheck(variable, b2), Seq((variable, BoundRemoval(b2))), Unavailable, s"${variable.toString(ps)} >= $med")
  )
}

final class Split() extends BranchHeuristic {

  override def toString = "split"

  def compute(s: MAC, ps: ProblemState): ProblemState = ps

  override def branch(variable: Variable, domain: Domain, ps: ProblemState): Either[Outcome, (ProblemState, Decision, Decision)] = {
    Right(Split.splitAt(variable, domain, domain.median, ps))
  }

  def shouldRestart = false
}
