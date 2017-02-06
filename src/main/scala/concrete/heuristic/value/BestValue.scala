package concrete
package heuristic
package value

final class BestValue(fallback: ValueHeuristic) extends ValueHeuristic {

  def this(params: ParameterManager) = this{
    val valueHeuristicClass: Class[_ <: ValueHeuristic] =
      params.classInPackage("bestvalue.fallback", "concrete.heuristic.value", classOf[RandomBound])

    valueHeuristicClass.getConstructor(classOf[ParameterManager]).newInstance(params)
  }

  override def toString = "best";

  var best: Array[Int] = _

  def compute(p: Problem) {
    require(p.variables.zipWithIndex.forall { case (v, i) => v.id == i })

    best =
      p.variables.map(v => fallback.selectIndex(v, v.initDomain))
    // Nothing to compute
  }

  override def selectIndex(variable: Variable, domain: Domain) = {
    val value = best(variable.id)
    if (domain.present(value)) {
      value
    } else {
      fallback.selectIndex(variable, domain)
    }
  }

  def shouldRestart = false

  override def applyListeners(s: MAC): Unit = s.solutionListener = Some { sol: Map[Variable, Any] =>
    for ((variable, value: Int) <- sol) {
      best(variable.id) = value
    }
  }

}
