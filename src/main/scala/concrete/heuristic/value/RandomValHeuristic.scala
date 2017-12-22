package concrete
package heuristic
package value

import java.util.EventObject

import com.typesafe.scalalogging.LazyLogging

import scala.util.Random

class RandomValHeuristic(heuristics: Seq[BranchHeuristic], factors: Seq[Double], rand: Random)
  extends BranchHeuristic with LazyLogging {

  def branch(v: Variable, dom: Domain, ps: ProblemState): (Decision, Decision) = {
    var totalFact: Double = 0.0
    val r = rand.nextDouble()

    for ((h, f) <- (heuristics.dropRight(1), factors).zipped) {
      totalFact += f
      if (r < totalFact) {
        return h.branch(v, dom, ps)
      }
    }
    heuristics.last.branch(v, dom, ps)
  }

  override def compute(s: MAC, state: ProblemState): ProblemState = {
    heuristics.foldLeft(state)((ps, h) => h.compute(s, ps))
  }

  override def shouldRestart: Boolean = heuristics.size > 1 || heuristics.exists(_.shouldRestart)

  override def toString: String = s"Random ${(heuristics, factors).zipped.mkString(", ")}"

  override def event[S <: Outcome](e: EventObject, ps: S): S = heuristics.foldLeft(ps)((ps, h) => h.event(e, ps))
}
