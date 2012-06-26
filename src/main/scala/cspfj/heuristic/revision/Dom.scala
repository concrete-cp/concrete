package cspfj.heuristic.revision

import cspfj.Variable

final class Dom extends Key[Variable] {
  def getKey(v: Variable) = v.dom.size
}