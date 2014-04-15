package concrete.heuristic;

import java.util.Comparator
import concrete.Variable
import concrete.Problem
import scala.math.Ordering.DoubleOrdering
import scala.util.Random
import scala.annotation.tailrec
import concrete.ParameterManager

abstract class VariableHeuristic(params: ParameterManager) extends Ordering[Variable] {
  private val rb: Boolean =
    params.getOrElse("variableHeuristic.randomBreak", true)
  private val seed: Long =
    params.getOrElse("randomBreak.seed", 0L)

  private val rand = if (rb) Some(new Random(seed)) else None

  //def problem: Problem

  def select(problem: Problem): Option[Variable] =
    select(problem.variables.iterator.filter(_.dom.size > 1))

  @tailrec
  private def select(list: Iterator[Variable], best: Variable, ties: Int, rand: Random): Variable = {
    if (list.isEmpty) { best }
    else {
      val current = list.next
      val comp = compare(current, best)

      if (comp > 0) {
        select(list, current, 2, rand)
      } else if (comp == 0) {
        if (rand.nextDouble() * ties < 1) {
          select(list, current, ties + 1, rand)
        } else {
          select(list, best, ties + 1, rand)
        }

      } else {
        select(list, best, ties, rand)
      }
    }
  }

  def select(itr: Iterator[Variable]): Option[Variable] = {
    if (itr.isEmpty) {
      None
    } else {
      rand.map {
        rand => select(itr, itr.next, 2, rand)
      } orElse {
        Some(itr.maxBy(score))
      }
    }
  }

  def score(variable: Variable): Double

  def compare(v1: Variable, v2: Variable) = {
    //println(v1 + ", " + v2 + " : " + score(v1).compare(score(v2)))
    score(v1).compare(score(v2))
  }

}
