package concrete.heuristic.revision
import concrete.constraint.Constraint
import concrete.Variable
import concrete.ProblemState

final class DomCtr extends Key[Constraint] {
  def getKey(c: Constraint, s: ProblemState) = {
    val vars = c.scope
    var i = c.arity - 1
    var min = s(vars(i)).size
    i -= 1
    while (i >= 0) {
      min = math.min(min, s(vars(i)).size)
      i -= 1
    }
    min
  }
}