package cspfj.heuristic.revision
import cspfj.priorityqueues.Key
import cspfj.constraint.Constraint

final class Eval extends Key[Constraint] {
  def getKey(o: Constraint) = o.getEvaluation

  override def toString = "constraint.getEvaluation"
}