package concrete.heuristic;

import concrete.Variable
import concrete.Problem
import concrete.Domain
import concrete.ProblemState

final class Lexico extends BranchHeuristic {

  override def toString = "lexico";

  def compute(p: Problem) {
    // Nothing to compute
  }

  override def branch(variable: Variable, dom: Domain, ps: ProblemState) = {
    val h = dom.head
    new Branch(
      ps.assign(variable, h).toState,
      ps.remove(variable, h).toState,
      Seq(variable),
      s"${variable.toString(ps)} = $h",
      s"${variable.toString(ps)} /= $h")
  }

  def shouldRestart = false

}
