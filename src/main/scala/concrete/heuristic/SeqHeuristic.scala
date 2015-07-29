package concrete.heuristic;

import java.lang.reflect.InvocationTargetException
import concrete.Pair
import concrete.ParameterManager
import concrete.Problem
import concrete.Variable;
import concrete.ProblemState

final class SeqHeuristic(heuristics: List[Heuristic]) extends Heuristic {

  def selectPair(state: ProblemState) = {
    heuristics.iterator.map(_.selectPair(state)).collectFirst {
      case Some(pair) => pair
    }
  }

  override def toString = heuristics.toString

  def shouldRestart = heuristics.exists(_.shouldRestart)

}
