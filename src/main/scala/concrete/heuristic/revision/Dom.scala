package concrete.heuristic.revision

import concrete.ProblemState
import concrete.Variable

final class Dom extends Key[Variable] {
  def getKey(d: Variable, s: ProblemState) = s.dom(d).size
}