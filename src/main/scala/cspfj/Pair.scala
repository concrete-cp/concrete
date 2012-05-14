package cspfj

case class Pair(val variable: Variable, val index: Int) {
  def value = variable.dom.value(index)
  def assign() {
    variable.dom.setSingle(index)
  }
  def remove() {
    variable.dom.remove(index)
  }
}