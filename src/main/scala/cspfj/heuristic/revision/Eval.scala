package cspfj.heuristic.revision

import cspfj.constraint.Constraint

final class Eval extends Key[Constraint] {
  override def getKey(o: Constraint, e: Int) = e
  def getKey(o: Constraint) = throw new UnsupportedOperationException

  override def toString = "constraint.advise"
}