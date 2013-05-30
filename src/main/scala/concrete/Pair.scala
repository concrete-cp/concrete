package concrete

final case class Pair(val variable: Variable, val index: Int) {
  private val dom = variable.dom
  def value: Int = dom.value(index)
  def assign() {
    dom.setSingle(index)
  }
  def remove() {
    dom.remove(index)
  }
}
