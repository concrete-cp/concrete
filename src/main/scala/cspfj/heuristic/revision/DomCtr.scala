package cspfj.heuristic.revision
import cspfj.constraint.Constraint
import cspfj.Variable

final class DomCtr extends Key[Constraint] {
  def getKey(c: Constraint) = {
    val vars = c.scope
    var i = c.arity - 1
    var min = vars(i).dom.size
    i -= 1
    while (i >= 0) {
      min = math.min(min, vars(i).dom.size)
      i -= 1
    }
    min
  }
}