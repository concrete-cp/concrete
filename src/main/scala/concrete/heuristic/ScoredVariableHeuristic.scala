package concrete.heuristic;

import java.util.Comparator
import concrete.Variable
import concrete.Problem
import scala.math.Ordering.DoubleOrdering
import scala.util.Random
import scala.annotation.tailrec
import concrete.ParameterManager
import concrete.ProblemState
import concrete.Domain

abstract class ScoredVariableHeuristic(params: ParameterManager, decisionVariables: Array[Variable]) extends VariableHeuristic(params, decisionVariables) {

  //def problem: Problem

  @tailrec
  private def select(i: Int, best: Variable, bestScore: Double, ties: Int, state: ProblemState, rand: Random): Variable = {
    if (i < 0) {
      best
    } else {
      val current = decisionVariables(i)
      val dom = state.dom(current)
      if (dom.length == 1) {
        select(i - 1, best, bestScore, ties, state, rand)
      } else {
        val s = score(current, dom, state)
        val comp = java.lang.Double.compare(s, bestScore)

        if (comp > 0) {
          select(i - 1, current, s, 2, state, rand)
        } else if (comp == 0) {
          if (rand.nextDouble() * ties < 1) {
            select(i - 1, current, s, ties + 1, state, rand)
          } else {
            select(i - 1, best, bestScore, ties + 1, state, rand)
          }

        } else {
          select(i - 1, best, bestScore, ties, state, rand)
        }
      }
    }
  }

  @tailrec
  private def select(i: Int, best: Variable, bestScore: Double, state: ProblemState): Variable = {
    if (i < 0) {
      best
    } else {
      val current = decisionVariables(i)
      val dom = state.dom(current)
      if (dom.length == 1) {
        select(i - 1, best, bestScore, state)
      } else {
        val s = score(current, dom, state)
        val comp = java.lang.Double.compare(s, bestScore)
        if (comp > 0) {
          select(i - 1, current, s, state)
        } else {
          select(i - 1, best, bestScore, state)
        }
      }
    }
  }

  override def select(i: Int, state: ProblemState): Variable = {
    val v = decisionVariables(i)
    rand
      .map { r =>
        select(i - 1, v, score(v, state.dom(v), state), 2, state, r)
      }
      .getOrElse {
        select(i - 1, v, score(v, state.dom(v), state), state)
      }
  }

  def compare(v1: Variable, d1: Domain, v2: Variable, d2: Domain, state: ProblemState): Int =
    java.lang.Double.compare(score(v1, d1, state), score(v2, d2, state))

  def score(variable: Variable, domain: Domain, state: ProblemState): Double

}
