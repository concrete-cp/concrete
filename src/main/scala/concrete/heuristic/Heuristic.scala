package concrete
package heuristic
import concrete.Event

trait Heuristic {
  def branch(state: ProblemState): Option[Branch]
  def shouldRestart: Boolean
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