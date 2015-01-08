package concrete.heuristic;

import java.util.Comparator
import concrete.Variable
import concrete.Problem
import scala.math.Ordering.DoubleOrdering
import scala.util.Random
import scala.annotation.tailrec
import concrete.ParameterManager
import concrete.ProblemState

abstract class VariableHeuristic(params: ParameterManager) {
  private val rand: Random = {
    if (params.getOrElse("variableHeuristic.randomBreak", true)) {
      val seed = params.getOrElse("randomBreak.seed", 0L)
      new Random(seed)
    } else null

  }

  //def problem: Problem

  def select(problem: Problem, state: ProblemState): Option[Variable] =
    select(problem.variables.iterator.filter(state.dom(_).size > 1), state)

  @tailrec
  private def select(list: Iterator[Variable], best: Variable, ties: Int, rand: Random, state: ProblemState): Variable = {
    if (list.hasNext) {
      val current = list.next
      val comp = score(current, state).compareTo(score(best, state))

      if (comp > 0) {
        select(list, current, 2, rand, state)
      } else if (comp == 0) {
        if (rand.nextDouble() * ties < 1) {
          select(list, current, ties + 1, rand, state)
        } else {
          select(list, best, ties + 1, rand, state)
        }

      } else {
        select(list, best, ties, rand, state)
      }
    } else {
      best
    }
  }

  def select(itr: Iterator[Variable], state: ProblemState): Option[Variable] = {
    if (itr.isEmpty) {
      None
    } else if (rand ne null) {
      Some(select(itr, itr.next, 2, rand, state))
    } else {

      Some(itr.maxBy(score(_, state)))

    }
  }

  def score(variable: Variable, state: ProblemState): Double

}
