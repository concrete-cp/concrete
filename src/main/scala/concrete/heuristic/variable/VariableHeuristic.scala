package concrete
package heuristic
package variable

import scala.annotation.tailrec
import scala.util.Random

abstract class VariableHeuristic(params: ParameterManager, val decisionVariables: Array[Variable]) {
  protected val rand: Option[Random] = {
    if (params.getOrElse("heuristic.variable.randomBreak", true)) {
      val seed = params.getOrElse("randomBreak.seed", 0L) + params.getOrElse("iteration", 0)
      Some(new Random(seed))
    } else {
      None
    }
  }

  final def select(state: ProblemState): Option[Variable] = {
    var i = decisionVariables.length - 1
    while (i >= 0 && state.dom(decisionVariables(i)).isAssigned) i -= 1
    if (i < 0) None else Some(select(i, state))
  }

  @tailrec
  private def select(i: Int, best: Variable, bestDomain: Domain, ties: Int, state: ProblemState, rand: Random): Variable = {
    if (i < 0) {
      best
    } else {
      val current = decisionVariables(i)
      val dom = state.dom(current)
      if (dom.isAssigned) {
        select(i - 1, best, bestDomain, ties, state, rand)
      } else {
        val comp = compare(current, dom, best, bestDomain, state)

        if (comp > 0) {
          select(i - 1, current, dom, 2, state, rand)
        } else if (comp == 0) {
          if (rand.nextDouble() * ties < 1) {
            select(i - 1, current, dom, ties + 1, state, rand)
          } else {
            select(i - 1, best, bestDomain, ties + 1, state, rand)
          }

        } else {
          select(i - 1, best, bestDomain, ties, state, rand)
        }
      }
    }
  }

  @tailrec
  private def select(i: Int, best: Variable, bestDomain: Domain, state: ProblemState): Variable = {
    if (i < 0) {
      best
    } else {
      val current = decisionVariables(i)
      val dom = state.dom(current)
      if (dom.isAssigned) {
        select(i - 1, best, bestDomain, state)
      } else {
        val comp = compare(current, dom, best, bestDomain, state) //score(current, dom, state).compareTo(score(best, dom, state))

        if (comp > 0) {
          select(i - 1, current, dom, state)
        } else {
          select(i - 1, best, bestDomain, state)
        }
      }
    }
  }

  protected def select(i: Int, state: ProblemState): Variable = {
    val v = decisionVariables(i)
    rand
      .map { r =>
        select(i - 1, v, state.dom(v), 2, state, r)
      }
      .getOrElse {
        select(i - 1, v, state.dom(v), state)
      }
  }

  def compare(v1: Variable, d1: Domain, v2: Variable, d2: Domain, state: ProblemState): Int

  def shouldRestart = rand.isDefined

  def applyListeners(s: MAC): Unit = ()

}
