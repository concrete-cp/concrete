package concrete.heuristic.revision

import concrete.constraint.Constraint

final class SimpleEval extends Key[Constraint] {
  def getKey(o: Constraint) = o.simpleEvaluation

  override def toString = "constraint.simple"
}