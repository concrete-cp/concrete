package concrete
package heuristic
package value;

final class PhaseSaving(heuristic: ValueHeuristic) extends ValueHeuristic {

  private var previous: Array[Int] = _

  def this(params: ParameterManager) = this{
    val valueHeuristicClass: Class[_ <: ValueHeuristic] =
      params.classInPackage("phasesaving.heuristic", "concrete.heuristic.value", classOf[Lexico])
    valueHeuristicClass.getConstructor(classOf[ParameterManager]).newInstance(params)
  }

  override def toString = "best";

  def compute(p: Problem) {
    heuristic.compute(p)
    previous = p.variables.map(v => heuristic.selectIndex(v, v.initDomain))
  }

  override def selectIndex(variable: Variable, domain: Domain) = {
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
