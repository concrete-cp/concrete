package concrete.heuristic.restart

import concrete.ParameterManager
import concrete.Problem

class Geometric(params: ParameterManager, problem: Problem) extends RestartStrategy {
  val btGrowth: Double = params.getOrElse("geometric.growth", 1.2)

  val base: Int = params.getOrElse("geometric.base", 100)

  var maxBacktracks: Int = base

  def reset(): Unit = maxBacktracks = base

  def nextRun(): Option[Int] = {
    val bt = maxBacktracks
    maxBacktracks = (maxBacktracks * btGrowth).toInt
    Some(bt)
  }

}