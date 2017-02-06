package concrete
package heuristic
package value

final class Lexico(pm: ParameterManager) extends ValueHeuristic {

  override def toString = "lexico";

  def compute(p: Problem) {
    // Nothing to compute
  }

  override def selectIndex(variable: Variable, dom: Domain) = dom.head

  def shouldRestart = false

}
