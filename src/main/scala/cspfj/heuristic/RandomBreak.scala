package cspfj.heuristic;

import cspfj.problem.Problem
import cspfj.problem.Variable
import cspfj.util.TieManager
import scala.util.Random
import scala.annotation.tailrec

trait RandomBreak extends VariableHeuristic {

  val rand = new Random(0)

  override def select(itr: List[Variable]): Option[Variable] = {
    if (itr.isEmpty) None
    else {

      @tailrec
      def select(itr: List[Variable], best: Variable, ties: Int): Variable = {
        if (itr.isEmpty) best
        else {
          val current = itr.head
          val comp = compare(current, best)
          if (comp > 0) select(itr.tail, current, 2)
          else if (comp == 0) {
            if (rand.nextDouble * ties < 1)
              select(itr.tail, current, ties + 1)
            else
              select(itr.tail, best, ties + 1)

          } else select(itr.tail, current, ties)
        }
      }

      Some(select(itr.tail, itr.head, 2))

    }
  }

}
