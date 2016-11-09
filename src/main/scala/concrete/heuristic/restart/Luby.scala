package concrete.heuristic.restart

import concrete.ParameterManager
import concrete.Problem

class Luby(params: ParameterManager, problem: Problem) extends RestartStrategy {

  val scaleFactor = params.getOrElse("luby.base", 100)
  val geometricalFactor = params.getOrElse("luby.growth", 2.0)

  var nbRestart = 0

  private def geometricalSum(value: Double, exponent: Int): Double = {
    (math.pow(value, exponent) - 1) / (value - 1);
  }

  private def checkedIntValue(i: Double) = {
    require(i <= Int.MaxValue)
    i.intValue
  }

  private def getLasVegasCoef(i: Int): Int = {
    val log = Math.log(i * (geometricalFactor - 1) + 1) / Math.log(geometricalFactor)
    val k = Math.floor(log).toInt;
    if (log == k) {
      checkedIntValue(math.pow(geometricalFactor, k - 1))
    } else {
      //recursion
      getLasVegasCoef(i - checkedIntValue(geometricalSum(geometricalFactor, k)));
    }
  }

  def reset(): Unit = { nbRestart = 0 }

  def nextRun(): Int = {
    nbRestart += 1
    getLasVegasCoef(nbRestart) * scaleFactor
  }
}