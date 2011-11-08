package cspfj.heuristic;

import cspfj.problem.Problem
import cspfj.problem.Variable
import cspfj.util.TieManager;
import scala.util.Random

object RandomBreak {
  val RAND = new Random(0)
}

trait RandomBreak extends VariableHeuristic {

  RandomBreak.RAND.setSeed(0)

  override def select(itr: Iterator[Variable]): Option[Variable] = {
    if (itr.isEmpty) None
    else {
      var best = itr.next
      var nbTies = 2
      while (itr.hasNext) {
        val current = itr.next
        val comp = compare(current, best)
        if (comp > 0) {
          nbTies = 2
          best = current
        } else if (comp == 0) {
          if (RandomBreak.RAND.nextDouble * nbTies < 1) {
            best = current
          }
          nbTies += 1
        }
      }
      Some(best)
    }
  }

}
