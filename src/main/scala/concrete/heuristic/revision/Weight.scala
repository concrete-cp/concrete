package concrete.heuristic.revision

import concrete.ProblemState
import concrete.Variable
import concrete.constraint.Constraint

final class Weight extends Key[Constraint] {
  def getKey(c: Constraint, s: ProblemState) = c.weight
}