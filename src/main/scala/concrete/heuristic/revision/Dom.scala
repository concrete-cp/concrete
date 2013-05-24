package concrete.heuristic.revision

import concrete.Variable

final class Dom extends Key[Variable] {
  def getKey(v: Variable) = v.dom.size
}