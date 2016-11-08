package concrete.heuristic.restart

import concrete.ParameterManager
import concrete.Problem

class Luby(params: ParameterManager, problem: Problem) extends RestartStrategy {

  val scaleFactor = 100
  val geometricalFactor = 2

  var nbRestart = 0

  private def geometricalSum(value: Int, exponent: Int): BigInt = {
    (BigInt(value).pow(exponent) - 1) / (value - 1);
  }

  private def checkedIntValue(i: BigInt) = {
    require(i <= Int.MaxValue)
    i.intValue
  }

  private def getLasVegasCoef(i: Int): Int = {
    val log = Math.log(i * (geometricalFactor - 1) + 1) / Math.log(geometricalFactor)
    val k = Math.floor(log).toInt;
    if (log == k) {
      checkedIntValue(BigInt(geometricalFactor).pow(k - 1))
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