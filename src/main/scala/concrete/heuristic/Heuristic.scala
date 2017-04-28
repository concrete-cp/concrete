package concrete
package heuristic
import concrete.Event
import java.util.EventObject
import java.util.EventListener

object Heuristic {
  def default(pm: ParameterManager, decision: Array[Variable]): Heuristic = {
    pm.getRaw("heuristic").getOrElse(classOf[CrossHeuristic]) match {
      case heuristicName: String =>
        instantiate(pm.classInPackage(heuristicName, "concrete.heuristic"), pm, decision)
      case heuristicClass: Class[_] =>
        instantiate(heuristicClass, pm, decision)
      case heuristic: Heuristic =>
        heuristic
    }
  }

  private def instantiate(heuristicClass: Class[_], pm: ParameterManager, decision: Array[Variable]) = {
    heuristicClass
      .getMethod("apply", classOf[ParameterManager], classOf[Array[Variable]])
      .invoke(null, pm, decision)
      .asInstanceOf[Heuristic]
  }
}

trait Heuristic extends HeuristicListener {
  def branch(state: ProblemState): Option[Branch]
  def shouldRestart: Boolean
  def decisionVariables: Seq[Variable]
  def compute(p: Problem): Unit

}

trait HeuristicListener extends EventListener {
  def event(e: EventObject): Unit
}

class Branch(
    val b1: ProblemState,
    val c1: Seq[(Variable, Event)],
    val b2: ProblemState,
    val c2: Seq[(Variable, Event)],
    _b1Desc: => String, _b2Desc: => String) {
  lazy val b1Desc = _b1Desc
  lazy val b2Desc = _b2Desc
}

case class NewSolutionEvent(sol: Map[Variable, Int]) extends EventObject(sol)
case class ContradictionEvent(c: Contradiction) extends EventObject(c)
