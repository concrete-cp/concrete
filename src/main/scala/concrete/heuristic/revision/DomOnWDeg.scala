package concrete.heuristic.revision

import concrete.Variable
import concrete.heuristic.WDeg

final class DomOnWDeg extends Key[Variable] {
  def getKey(v: Variable) = v.dom.size / math.max(1, v.getWDegEntailed)
}