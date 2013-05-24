package concrete.heuristic.revision
import concrete.constraint.Constraint
import concrete.Variable

final class PiDom extends Key[Constraint] {
  def getKey(c: Constraint) = Key.prod(c.scope)

}