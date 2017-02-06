package concrete
package heuristic
package value

final class MedValue(pm: ParameterManager) extends ValueHeuristic {

  def score(variable: Variable, domain: Domain, value: Int) = domain.median - value

  override def toString = "median";

  def compute(p: Problem) {
    // Nothing to compute
  }

  override def selectIndex(variable: Variable, domain: Domain) = domain.median

  def shouldRestart = false
}
