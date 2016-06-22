package concrete.heuristic;

import scala.annotation.tailrec

import com.typesafe.scalalogging.LazyLogging

import concrete.Domain
import concrete.ParameterManager
import concrete.ProblemState
import concrete.Variable

abstract class ScoredVariableHeuristic(params: ParameterManager, decisionVariables: Array[Variable]) extends VariableHeuristic(params, decisionVariables)
    with LazyLogging {

  //def problem: Problem

  @tailrec
  private def selectRand(i: Int, best: Variable, bestScore: Double, ties: Int, state: ProblemState, rand: Double): Variable = {
    if (i < 0) {
      logger.debug(s"$best: $bestScore")
      best
    } else {
      val current = decisionVariables(i)
      val dom = state.dom(current)
      if (dom.isAssigned) {
        selectRand(i - 1, best, bestScore, ties, state, rand)
      } else {
        val s = score(current, dom, state)

        if (s > bestScore) {
          selectRand(i - 1, current, s, 2, state, rand)
        } else if (s < bestScore) {
          selectRand(i - 1, best, bestScore, ties, state, rand)
        } else if (rand * ties < 1) {
          selectRand(i - 1, current, s, ties + 1, state, rand)
        } else {
          selectRand(i - 1, best, bestScore, ties + 1, state, rand)
        }

      }
    }
  }

  @tailrec
  private def selectFirst(i: Int, best: Variable, bestScore: Double, state: ProblemState): Variable = {
    if (i < 0) {
      best
    } else {
      val current = decisionVariables(i)
      val dom = state.dom(current)
      if (dom.isAssigned) {
        selectFirst(i - 1, best, bestScore, state)
      } else {
        val s = score(current, dom, state)
        if (s > bestScore) {
          selectFirst(i - 1, current, s, state)
        } else {
          selectFirst(i - 1, best, bestScore, state)
        }
      }
    }
  }

  override def select(i: Int, state: ProblemState): Variable = {
    val v = decisionVariables(i)
    rand
      .map { r =>
        selectRand(i - 1, v, score(v, state.dom(v), state), 2, state, r.nextDouble())
      }
      .getOrElse {
        selectFirst(i - 1, v, score(v, state.dom(v), state), state)
      }
  }

  def compare(v1: Variable, d1: Domain, v2: Variable, d2: Domain, state: ProblemState): Int =
    java.lang.Double.compare(score(v1, d1, state), score(v2, d2, state))

  def score(variable: Variable, domain: Domain, state: ProblemState): Double

}
