package concrete
package heuristic
package value

final class RevLexico(pm: ParameterManager) extends ValueHeuristic {

  def score(variable: Variable, dom: Domain, value: Int) = value

  override def toString = "lexico";

  def compute(p: Problem) {
    // Nothing to compute
  }

  override def selectIndex(variable: Variable, dom: Domain) = dom.last
  def shouldRestart = false
}
