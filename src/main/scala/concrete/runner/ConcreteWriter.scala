package concrete.runner

import concrete.ParameterManager
import cspom.StatisticsManager

trait ConcreteWriter {
  var end: Result = Unfinished()

  var lastSolution: Option[String] = None

  def stats: StatisticsManager

  def parameters(params: ParameterManager): Unit

  def problem(problem: String): Unit

  def solution(solution: String, obj: Option[Any]): Unit = {
    lastSolution = Some(solution)
    printSolution(solution, obj)
  }

  def printSolution(solution: String, obj: Option[Any]): Unit

  def error(e: Throwable): Unit

  def disconnect(): Unit = disconnect(end)

  def disconnect(status: Result): Unit
}
