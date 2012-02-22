package cspfj.heuristic.revision
import cspfj.constraint.Constraint
import cspfj.priorityqueues.Key
import cspfj.problem.Variable

final class PiDom extends Key[Constraint] {
  def getKey(c: Constraint) = Key.prod(c.scope)

}