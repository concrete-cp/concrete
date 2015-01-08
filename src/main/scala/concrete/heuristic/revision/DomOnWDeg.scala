package concrete.heuristic.revision

import concrete.Variable
import concrete.heuristic.WDeg
import concrete.ProblemState

final class DomOnWDeg extends Key[Variable] {
  def getKey(v: Variable, s: ProblemState) = s.dom(v).size / math.max(1, v.getWDegEntailed)
}