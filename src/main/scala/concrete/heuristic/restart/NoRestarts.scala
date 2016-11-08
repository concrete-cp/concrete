package concrete.heuristic.restart

import concrete.ParameterManager
import concrete.Problem

class NoRestarts(params: ParameterManager, problem: Problem) extends RestartStrategy {
  def nextRun(): Int = -1
  def reset(): Unit = ()
}