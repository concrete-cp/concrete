package concrete.heuristic.revision

import concrete.constraint.Constraint
import concrete.ProblemState

final class Eval extends Key[Constraint] {
  override def getKey(o: Constraint, s: ProblemState, e: Int) = e
  def getKey(o: Constraint, s: ProblemState) = throw new UnsupportedOperationException

  override def toString = "constraint.advise"
}