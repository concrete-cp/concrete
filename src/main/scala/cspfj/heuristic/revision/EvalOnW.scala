package cspfj.heuristic.revision
import cspfj.priorityqueues.Key
import cspfj.constraint.Constraint

final class EvalOnW extends Key[Constraint] {
  def getKey(o: Constraint) = o.getEvaluation / o.weight

  override def toString = "eval/w"
}