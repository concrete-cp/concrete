package cspfj.heuristic;

import cspfj.Problem
import cspfj.Variable
import cspfj.util.TieManager
import scala.util.Random
import scala.annotation.tailrec

trait RandomBreak extends VariableHeuristic {

  val rand = new Random(0)

  override def select(itr: List[Variable]): Option[Variable] = {
    if (itr.isEmpty) None
    else {

      @tailrec
      def select(list: List[Variable], best: Variable, ties: Int): Variable = {
        if (list.isEmpty) best
        else {
          val current = list.head
          val comp = compare(current, best)
          
          if (comp > 0) select(list.tail, current, 2)
          else if (comp == 0) {
            if (rand.nextDouble * ties < 1)
              select(list.tail, current, ties + 1)
            else
              select(list.tail, best, ties + 1)

          } else select(list.tail, best, ties)
        }
      }

      Some(select(itr.tail, itr.head, 2))

    }
  }

}
