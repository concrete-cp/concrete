package concrete.heuristic.restart

import concrete.ParameterManager
import concrete.Problem

class Arithmetic(params: ParameterManager, problem: Problem) extends RestartStrategy {
  val growth: Int = params.getOrElse("arithmetic.growth", 100)

  val base: Int = params.getOrElse("arithmetic.base", 100)

  var maxBacktracks: Int = base

  def reset() = maxBacktracks = base

  def nextRun() = {
    val bt = maxBacktracks
    maxBacktracks += growth
    bt
  }

}