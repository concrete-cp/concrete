package concrete
package heuristic
package value

final class RevLexico(pm: ParameterManager) extends ValueHeuristic {


  override def toString: String = "rev-lexico"

  def compute(s: MAC, ps: ProblemState): ProblemState = ps

  override def selectIndex(variable: Variable, dom: Domain): Int = dom.last

  def shouldRestart = false
}
