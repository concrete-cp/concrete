package concrete.heuristic.revision

import concrete.constraint.Constraint
import concrete.ProblemState

final class Zero extends Key[Constraint] {
  def getKey(o: Constraint, s: ProblemState) = 0

  override def toString = "zero"
}