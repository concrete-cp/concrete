package concrete.heuristic.revision
import concrete.constraint.Constraint
import concrete.Variable
import concrete.ProblemState

final class PiDomOnW extends Key[Constraint] {
  def getKey(c: Constraint, s: ProblemState) = Key.prod(c.scope, s) / c.weight

}