package concrete.heuristic.restart

import concrete.ParameterManager
import concrete.Problem

class Geometric(params: ParameterManager, problem: Problem) extends RestartStrategy {
  val btGrowth: Double = params.getOrElse("geometric.btGrowth", 1.5)

  var maxBacktracks: Int = initBT()

  private def initBT(): Int =
    if (problem.variables.isEmpty) 10
    else
      math.max(10, math.log(problem.variables.map(_.initDomain.size).max).toInt)

  def reset() = maxBacktracks = initBT()

  def nextRun() = {
    val bt = maxBacktracks
    maxBacktracks = (maxBacktracks * btGrowth).toInt
    bt
  }

}