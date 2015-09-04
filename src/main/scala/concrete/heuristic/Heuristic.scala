package concrete.heuristic;

import concrete.ProblemState
import concrete.Variable

trait Heuristic {
  def branch(state: ProblemState): Option[Branch]
  def shouldRestart: Boolean
}

class Branch(
    val b1: ProblemState,
    val b2: ProblemState,
    val changed: Seq[Variable],
    _b1Desc: => String, _b2Desc: => String) {
  lazy val b1Desc = _b1Desc
  lazy val b2Desc = _b2Desc
} 