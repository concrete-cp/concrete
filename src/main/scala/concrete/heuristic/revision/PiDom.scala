package concrete.heuristic.revision
import concrete.constraint.Constraint
import concrete.Variable
import concrete.ProblemState

final class PiDom extends Key[Constraint] {
  def getKey(c: Constraint, s: ProblemState) = Key.prod(c.scope, s)

}