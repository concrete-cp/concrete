package concrete
package heuristic
package value

final class PhaseSaving(heuristic: ValueHeuristic) extends ValueHeuristic {

  private var previous: Array[Int] = _

  def this(params: ParameterManager) = this {
    val valueHeuristicClass: Class[_ <: ValueHeuristic] =
      params.classInPackage("phasesaving.heuristic", "concrete.heuristic.value", classOf[Lexico])
    valueHeuristicClass.getConstructor(classOf[ParameterManager]).newInstance(params)
  }

  override def toString = "phase-saving"

  def compute(s: MAC, ps: ProblemState): ProblemState = {
    previous = s.problem.variables.map(v => heuristic.selectIndex(v, v.initDomain))
    heuristic.compute(s, ps)
  }

  override def selectIndex(variable: Variable, domain: Domain): Int = {
    val value = previous(variable.id)
    if (domain.present(value)) {
      value
    } else {
      val selected = heuristic.selectIndex(variable, domain)
      previous(variable.id) = selected
      selected
    }
  }

  def shouldRestart = false
}
