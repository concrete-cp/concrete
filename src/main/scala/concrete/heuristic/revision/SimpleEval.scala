package concrete.heuristic.revision

import concrete.constraint.Constraint
import concrete.ProblemState

final class SimpleEval extends Key[Constraint] {
  def getKey(o: Constraint, s: ProblemState) = o.simpleEvaluation

  override def toString = "constraint.simple"
}