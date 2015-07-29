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

abstract class VariableHeuristic(params: ParameterManager, val decisionVariables: List[Variable]) {
  protected val rand: Random = {
    if (params.getOrElse("heuristic.variable.randomBreak", true)) {
      val seed = params.getOrElse("randomBreak.seed", 0L)
      new Random(seed)
    } else null

  }

  //def problem: Problem

  final def select(state: ProblemState): Option[Variable] =
    select(decisionVariables.dropWhile { v => state.dom(v).length == 1 }, state)
  //.orElse(select(problem.variables.dropWhile { v => state.dom(v).size == 1 }, state))

  @tailrec
  private def select(list: List[Variable], best: Variable, bestDomain: Domain, ties: Int, state: ProblemState): Variable = {
    if (list.isEmpty) {
      best
    } else {
      val current :: tail = list
      val dom = state.dom(current)
      if (dom.length == 1) {
        select(tail, best, bestDomain, ties, state)
      } else {
        val comp = compare(current, dom, best, bestDomain, state)

        if (comp > 0) {
          select(tail, current, dom, 2, state)
        } else if (comp == 0) {
          if (rand.nextDouble() * ties < 1) {
            select(tail, current, dom, ties + 1, state)
          } else {
            select(tail, best, bestDomain, ties + 1, state)
          }

        } else {
          select(tail, best, bestDomain, ties, state)
        }
      }
    }
  }

  @tailrec
  private def select(list: List[Variable], best: Variable, bestDomain: Domain, state: ProblemState): Variable = {
    if (list.isEmpty) {
      best
    } else {
      val current :: tail = list
      val dom = state.dom(current)
      if (dom.length == 1) {
        select(tail, best, bestDomain, state)
      } else {
        val comp = compare(current, dom, best, bestDomain, state) //score(current, dom, state).compareTo(score(best, dom, state))

        if (comp > 0) {
          select(tail, current, dom, state)
        } else {
          select(tail, best, bestDomain, state)
        }
      }
    }
  }

  protected def select(list: List[Variable], state: ProblemState): Option[Variable] = list match {
    case Nil =>
      None
    case h :: t =>
      if (rand ne null) {
        Some(select(t, h, state.dom(h), 2, state))
      } else {
        Some(select(t, h, state.dom(h), state))
      }
  }

  def compare(v1: Variable, d1: Domain, v2: Variable, d2: Domain, state: ProblemState): Int

  def shouldRestart = rand ne null
  //def score(variable: Variable, domain: Domain, state: ProblemState): Double

}
