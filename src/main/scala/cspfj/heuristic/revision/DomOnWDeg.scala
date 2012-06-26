package cspfj.heuristic.revision

import cspfj.Variable
import cspfj.heuristic.WDeg

final class DomOnWDeg extends Key[Variable] {
  def getKey(v: Variable) = v.dom.size / math.max(1, v.getWDeg)
}