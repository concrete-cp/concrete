package concrete
package heuristic
package value

final class MedValue(pm: ParameterManager) extends ValueHeuristic {

  def score(variable: Variable, domain: Domain, value: Int) = domain.median - value

  override def toString = "median";

  def compute(s: MAC, ps: ProblemState): ProblemState = ps

  override def selectIndex(variable: Variable, domain: Domain): Int = domain.median

  def shouldRestart = false
}
