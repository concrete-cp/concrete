package cspfj
import cspfj.problem.Variable

case class Pair(val variable: Variable, val index: Int) {
  def value = variable.dom.value(index)
}