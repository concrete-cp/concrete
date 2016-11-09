package concrete.heuristic.restart

import concrete.ParameterManager
import concrete.Problem

class Geometric(params: ParameterManager, problem: Problem) extends RestartStrategy {
  val btGrowth: Double = params.getOrElse("geometric.growth", 1.2)

  val base: Int = params.getOrElse("geometric.base", 100)

  var maxBacktracks: Int = base

  def reset() = maxBacktracks = base

  def nextRun() = {
    val bt = maxBacktracks
    maxBacktracks = (maxBacktracks * btGrowth).toInt
    bt
  }

}