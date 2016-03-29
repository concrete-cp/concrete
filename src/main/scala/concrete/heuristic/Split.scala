package concrete.heuristic;

import concrete.Domain
import concrete.ParameterManager
import concrete.Problem
import concrete.ProblemState
import concrete.Variable

object Split {
  def splitAt(variable: Variable, med: Int, ps: ProblemState) = {
    new Branch(
      ps.removeFrom(variable, med).toState,
      ps.removeUntil(variable, med).toState,
      Seq(variable),
      s"${variable.toString(ps)} < $med", s"${variable.toString(ps)} >= $med")
  }
}

final class Split(pm: ParameterManager) extends BranchHeuristic {

  override def toString = "split";

  def compute(p: Problem) {
    // Nothing to compute
  }

  override def branch(variable: Variable, domain: Domain, ps: ProblemState) = {
    val med = domain.median
    Split.splitAt(variable, med, ps)
  }

  def shouldRestart = false
}
