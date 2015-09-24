package concrete.heuristic;

import concrete.Variable
import concrete.Problem
import concrete.Domain
import cspom.StatisticsManager
import concrete.ProblemState
import concrete.ParameterManager

final class RevSplit(pm: ParameterManager) extends BranchHeuristic {

  override def toString = "rev-split";

  def compute(p: Problem) {
    // Nothing to compute
  }

  override def branch(variable: Variable, domain: Domain, ps: ProblemState) = {
    val med = domain.median
    new Branch(
      ps.removeUntil(variable, med).toState,
      ps.removeFrom(variable, med).toState,
      Seq(variable),
      s"${variable.toString(ps)} >= $med",
      s"${variable.toString(ps)} < $med")
  }

  def shouldRestart = false
}
