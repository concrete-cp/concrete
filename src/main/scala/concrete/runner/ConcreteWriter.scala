package concrete.runner

import concrete.BuildInfo
import cspom.StatisticsManager

trait ConcreteWriter {
  var end: RunnerResult = Unfinished()

  var lastSolution: Option[String] = None

  def stats: StatisticsManager

  def solution(solution: String, obj: Seq[(String, Any)]): Unit = {
    lastSolution = Some(solution)
    printSolution(solution, obj)
  }

  def printSolution(solution: String, obj: Seq[(String, Any)]): Unit

  def error(e: Throwable): Unit

  def disconnect(): Unit = disconnect(end)

  def disconnect(status: RunnerResult): Unit

  def version: String = s"${BuildInfo.version}.${BuildInfo.buildTime}"
}
