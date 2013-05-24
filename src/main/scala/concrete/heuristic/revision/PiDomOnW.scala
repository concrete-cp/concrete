package concrete.heuristic.revision
import concrete.constraint.Constraint
import concrete.Variable

final class PiDomOnW extends Key[Constraint] {
  def getKey(c: Constraint) = Key.prod(c.scope) / c.weight

}