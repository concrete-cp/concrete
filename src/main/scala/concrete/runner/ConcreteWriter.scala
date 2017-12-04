package concrete.runner

import concrete.BuildInfo
import cspom.StatisticsManager

trait ConcreteWriter {
  var end: Result = Unfinished()

  var lastSolution: Option[String] = None

  def stats: StatisticsManager

  def solution(solution: String, obj: Option[Any]): Unit = {
    lastSolution = Some(solution)
    printSolution(solution, obj)
  }

  def printSolution(solution: String, obj: Option[Any]): Unit

  def error(e: Throwable): Unit

  def disconnect(): Unit = disconnect(end)

  def disconnect(status: Result): Unit

  def version: String = s"${BuildInfo.version}.${BuildInfo.buildTime}"
}
