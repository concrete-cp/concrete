package concrete.heuristic;

import concrete.Variable
import concrete.Problem
import scala.util.Random
import concrete.Domain

final class RandomValue extends ValueHeuristic {

  def score(variable: Variable, domain: Domain, index: Int) = -index

  override def toString = "lexico";

  def compute(p: Problem) {
    // Nothing to compute
  }

  private val rand = new Random(0)

  @annotation.tailrec
  private def select(list: Iterator[Int], best: Int, ties: Int): Int = {
    if (list.isEmpty) { best }
    else {
      val current = list.next

      if (rand.nextDouble() * ties < 1) {
        select(list, current, ties + 1)
      } else {
        select(list, best, ties + 1)
      }
    }
  }

  override def selectIndex(variable: Variable, dom: Domain) = {
    dom(rand.nextInt(dom.size))
  }

}
