package concrete
package heuristic

import java.util.{EventListener, EventObject}

object Heuristic {
  def default(pm: ParameterManager, decision: Seq[Variable]): Heuristic = {
    pm.getRaw("heuristic").getOrElse(classOf[CrossHeuristic]) match {
      case heuristicName: String =>
        instantiate(pm.classInPackage(heuristicName, "concrete.heuristic"), pm, decision)
      case heuristicClass: Class[_] =>
        instantiate(heuristicClass, pm, decision)
      case heuristic: Heuristic =>
        heuristic
    }
  }

  private def instantiate(heuristicClass: Class[_], pm: ParameterManager, decision: Seq[Variable]) = {
    heuristicClass
      .getMethod("apply", classOf[ParameterManager], classOf[Seq[Variable]])
      .invoke(null, pm, decision)
      .asInstanceOf[Heuristic]
  }
}

trait Heuristic extends HeuristicListener {
  def branch(state: ProblemState, candidates: Seq[Variable]): Option[Branch]

  def shouldRestart: Boolean

  def decisionVariables: Seq[Variable]

  def compute(s: MAC, state: ProblemState): ProblemState
}

trait HeuristicListener extends EventListener {
  def event(e: EventObject): Unit
}

class Branch(
              val b1: ProblemState,
              val c1: Seq[(Variable, Event)],
              val b2: Outcome,
              val c2: Seq[(Variable, Event)],
              _b1Desc: => String, _b2Desc: => String) {
  lazy val b1Desc: String = _b1Desc
  lazy val b2Desc: String = _b2Desc
}

case class NewSolutionEvent(sol: Map[Variable, Int]) extends EventObject(sol)

case class ContradictionEvent(c: Contradiction) extends EventObject(c)
