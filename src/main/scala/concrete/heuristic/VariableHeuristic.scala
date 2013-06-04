package concrete.heuristic;

import java.util.Comparator
import concrete.Variable
import concrete.Problem
import scala.math.Ordering.DoubleOrdering
import concrete.Parameter
import scala.util.Random
import scala.annotation.tailrec
import concrete.ParameterManager

object VariableHeuristic {
  @Parameter("variableHeuristic.randomBreak")
  var rb = true

  @Parameter("randomBreak.seed")
  var seed = 0L

  ParameterManager.register(this)
}

trait VariableHeuristic extends Ordering[Variable] {
  private val rand = if (VariableHeuristic.rb) new Random(VariableHeuristic.seed) else null

  //def problem: Problem

  def select(problem: Problem): Option[Variable] =
    select(problem.variables.iterator.filter(_.dom.size > 1))

  @tailrec
  private def select(list: Iterator[Variable], best: Variable, ties: Int): Variable = {
    if (list.isEmpty) { best }
    else {
      val current = list.next
      val comp = compare(current, best)

      if (comp > 0) {
        select(list, current, 2)
      } else if (comp == 0) {
        if (rand.nextDouble() * ties < 1) {
          select(list, current, ties + 1)
        } else {
          select(list, best, ties + 1)
        }

      } else {
        select(list, best, ties)
      }
    }
  }

  def select(itr: Iterator[Variable]): Option[Variable] = {
    if (itr.isEmpty) {
      None
    } else if (VariableHeuristic.rb) {
      Some(select(itr, itr.next, 2))
    } else {
      Some(itr.maxBy(score))
    }
  }

  def score(variable: Variable): Double

  def compare(v1: Variable, v2: Variable) = {
    //println(v1 + ", " + v2 + " : " + score(v1).compare(score(v2)))
    score(v1).compare(score(v2))
  }

}
