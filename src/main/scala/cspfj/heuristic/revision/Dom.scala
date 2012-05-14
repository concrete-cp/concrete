package cspfj.heuristic.revision
import cspfj.priorityqueues.Key
import cspfj.Variable

final class Dom extends Key[Variable] {
  def getKey(v: Variable) = v.dom.size
}