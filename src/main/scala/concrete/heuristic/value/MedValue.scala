package concrete
package heuristic
package value

final class MedValue() extends ValueHeuristic {
  override def toString = "median"

  def compute(s: MAC, ps: ProblemState): ProblemState = ps

  override def selectIndex(variable: Variable, domain: Domain): Int = domain.median

  def shouldRestart = false
}
