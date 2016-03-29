package concrete.heuristic.revision

import concrete.ProblemState
import concrete.constraint.Constraint

final class PiDom extends Key[Constraint] {
  def getKey(c: Constraint, s: ProblemState) = Key.prod(c.scope, s)

}