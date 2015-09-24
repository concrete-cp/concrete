package concrete.heuristic;

import concrete.Domain
import concrete.Problem
import concrete.ProblemState
import concrete.Variable

trait ValueHeuristic extends BranchHeuristic {
  def branch(variable: Variable, domain: Domain, ps: ProblemState): Branch = {
    val selected = selectIndex(variable, domain)
    new Branch(
      try { ps.assign(variable, selected).toState } catch {
        case e: Exception => throw new IllegalStateException(s"${variable.toString(ps)} = $selected is inconsistent", e)
      },
      try { ps.remove(variable, selected).toState } catch {
        case e: Exception => throw new IllegalStateException(s"${variable.toString(ps)} /= $selected is inconsistent", e)
      },
      Seq(variable),
      s"${variable.toString(ps)} = $selected",
      s"${variable.toString(ps)} /= $selected")
  }

  def selectIndex(variable: Variable, domain: Domain): Int
}

trait BranchHeuristic {
  def branch(variable: Variable, domain: Domain, ps: ProblemState): Branch

  def compute(problem: Problem): Unit

  def shouldRestart: Boolean
}