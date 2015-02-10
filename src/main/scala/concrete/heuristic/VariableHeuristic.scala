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

abstract class VariableHeuristic(params: ParameterManager) {
  private val rand: Random = {
    if (params.getOrElse("variableHeuristic.randomBreak", true)) {
      val seed = params.getOrElse("randomBreak.seed", 0L)
      new Random(seed)
    } else null

  }

  //def problem: Problem

  def select(problem: Problem, state: ProblemState): Option[Variable] =
    select(problem.variables, state)

  @tailrec
  private def select(list: List[Variable], best: Variable, ties: Int, state: ProblemState): Variable = {
    if (list.isEmpty) {
      best
    } else {
      val current :: tail = list
      val dom = state.dom(current)
      if (dom.size == 1) {
        select(tail, best, ties, state)
      } else {
        val comp = score(current, dom, state).compareTo(score(best, dom, state))

        if (comp > 0) {
          select(tail, current, 2, state)
        } else if (comp == 0) {
          if (rand.nextDouble() * ties < 1) {
            select(tail, current, ties + 1, state)
          } else {
            select(tail, best, ties + 1, state)
          }

        } else {
          select(tail, best, ties, state)
        }
      }
    }
  }

  @tailrec
  private def select(list: List[Variable], best: Variable, state: ProblemState): Variable = {
    if (list.isEmpty) {
      best
    } else {
      val current :: tail = list
      val dom = state.dom(current)
      if (dom.size == 1) {
        select(tail, best, state)
      } else {
        val comp = score(current, dom, state).compareTo(score(best, dom, state))

        if (comp > 0) {
          select(tail, current, state)
        } else {
          select(tail, best, state)
        }
      }
    }
  }

  def select(list: List[Variable], state: ProblemState): Option[Variable] = {
    if (list.isEmpty) {
      None
    } else if (rand ne null) {
      Some(select(list.tail, list.head, 2, state))
    } else {
      Some(select(list.tail, list.head, state))
    }
  }

  def score(variable: Variable, domain: Domain, state: ProblemState): Double

}
