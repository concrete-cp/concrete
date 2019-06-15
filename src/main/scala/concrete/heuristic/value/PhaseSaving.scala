package concrete
package heuristic
package value

import java.util.EventObject

import scala.collection.mutable

final class PhaseSaving() extends ValueSelector {

  private val previous = new mutable.HashMap[Variable, Int]()

  override def toString = "phase-saving"

  def compute(s: MAC, ps: ProblemState): ProblemState = ps

  override def select(ps: ProblemState, variable: Variable, candidates: Domain): (Outcome, Domain) = {
    val selection = previous.get(variable)
      .filter(candidates)
      .map(Singleton(_))
      .getOrElse(candidates)

    (ps, selection)
  }

  def shouldRestart = false

  override def event[S <: Outcome](e: EventObject, ps: S): S = {
    e match {
      case ae: AssignmentEvent => previous ++= ae.assignments
      case _ =>
    }
    ps
  }
}
