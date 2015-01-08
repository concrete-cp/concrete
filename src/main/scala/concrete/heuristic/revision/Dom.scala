package concrete.heuristic.revision

import concrete.Domain
import concrete.Variable
import concrete.ProblemState

final class Dom extends Key[Variable] {
  def getKey(d: Variable, s: ProblemState) = s.dom(d).size
}