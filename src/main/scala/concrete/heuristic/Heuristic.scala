package concrete
package heuristic

import java.util.{EventListener, EventObject}

import scala.util.{Failure, Random, Success, Try}

object Heuristic {
  def default(pm: ParameterManager, decision: Seq[Variable], rand: Random): Try[Heuristic] = {
    pm.getRaw("heuristic").getOrElse(classOf[CrossHeuristic]) match {
      case heuristicName: String =>
        instantiate(ParameterManager.classInPackage(heuristicName, "concrete.heuristic"), pm, decision, rand)
      case heuristicClass: Class[_] =>
        instantiate(heuristicClass, pm, decision, rand)
      case heuristic: Heuristic =>
        Success(heuristic)
      case other => Failure(new IllegalArgumentException(s"Heuristic $other cannot be used or instantiated"))
    }
  }

  private def instantiate(heuristicClass: Class[_], pm: ParameterManager, decision: Seq[Variable], rand: Random): Try[Heuristic] = {
    heuristicClass
      .getMethod("apply", classOf[ParameterManager], classOf[Seq[Variable]], classOf[Random])
      .invoke(null, pm, decision, rand)
      .asInstanceOf[Try[Heuristic]]
  }
}

trait Heuristic extends HeuristicListener {
  def branch(state: ProblemState, candidates: Seq[Variable]): Either[Outcome, (ProblemState, Decision, Decision)]

  def shouldRestart: Boolean

  def decisionVariables: Seq[Variable]

  def compute(s: MAC, state: ProblemState): ProblemState
}

trait HeuristicListener extends EventListener {
  def event[S <: Outcome](e: EventObject, ps: S): S
}

case class NewSolutionEvent(sol: Map[Variable, Int]) extends EventObject(sol)

case object ContradictionEvent extends EventObject(None)

case class AssignmentEvent(variable: Seq[Variable], value: Seq[Int]) extends EventObject(variable)

case class BadDecision(decision: Decision) extends EventObject(decision)