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

abstract class ScoredVariableHeuristic(params: ParameterManager) extends VariableHeuristic(params) {

  //def problem: Problem

  @tailrec
  private def select(list: List[Variable], best: Variable, bestScore: Double, ties: Int, state: ProblemState): Variable = {
    if (list.isEmpty) {
      best
    } else {
      val current :: tail = list
      val dom = state.dom(current)
      if (dom.length == 1) {
        select(tail, best, bestScore, ties, state)
      } else {
        val s = score(current, dom, state)
        val comp = java.lang.Double.compare(s, bestScore)

        if (comp > 0) {
          select(tail, current, s, 2, state)
        } else if (comp == 0) {
          if (rand.nextDouble() * ties < 1) {
            select(tail, current, s, ties + 1, state)
          } else {
            select(tail, best, bestScore, ties + 1, state)
          }

        } else {
          select(tail, best, bestScore, ties, state)
        }
      }
    }
  }

  @tailrec
  private def select(list: List[Variable], best: Variable, bestScore: Double, state: ProblemState): Variable = {
    if (list.isEmpty) {
      best
    } else {
      val current :: tail = list
      val dom = state.dom(current)
      if (dom.length == 1) {
        select(tail, best, bestScore, state)
      } else {
        val s = score(current, dom, state)
        val comp = java.lang.Double.compare(s, bestScore)
        if (comp > 0) {
          select(tail, current, s, state)
        } else {
          select(tail, best, bestScore, state)
        }
      }
    }
  }

  override def select(list: List[Variable], state: ProblemState): Option[Variable] = {
    if (list.isEmpty) {
      None
    } else if (rand ne null) {
      val h :: t = list
      Some(select(t, h, score(h, state.dom(h), state), 2, state))
    } else {
      val h :: t = list
      Some(select(t, h, score(h, state.dom(h), state), state))
    }
  }

  def compare(v1: Variable, d1: Domain, v2: Variable, d2: Domain, state: ProblemState): Int =
    java.lang.Double.compare(score(v1, d1, state), score(v2, d2, state))

  def score(variable: Variable, domain: Domain, state: ProblemState): Double

}
