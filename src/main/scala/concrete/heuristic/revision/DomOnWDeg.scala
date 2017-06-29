package concrete.heuristic.revision

import concrete.ProblemState
import concrete.Variable

final class DomOnWDeg extends Key[Variable] {
  def getKey(v: Variable, s: ProblemState) = s.dom(v).size / math.max(1, s.wDeg(v)) //.getWDegEntailed(s))
}