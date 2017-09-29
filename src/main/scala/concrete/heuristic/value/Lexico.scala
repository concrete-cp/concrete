package concrete
package heuristic
package value

final class Lexico(pm: ParameterManager) extends ValueHeuristic {

  override def toString = "lexico"

  def compute(s: MAC, ps: ProblemState): ProblemState = ps

  override def selectIndex(variable: Variable, dom: Domain): Int = dom.head

  def shouldRestart = false

}
