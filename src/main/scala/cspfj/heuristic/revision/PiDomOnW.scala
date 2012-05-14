package cspfj.heuristic.revision
import cspfj.constraint.Constraint
import cspfj.priorityqueues.Key
import cspfj.Variable

final class PiDomOnW extends Key[Constraint] {
  def getKey(c: Constraint) = Key.prod(c.scope) / c.weight

}