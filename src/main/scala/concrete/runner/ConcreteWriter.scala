package concrete.runner

import scala.util.Try

import concrete.ParameterManager
import cspom.StatisticsManager

trait ConcreteWriter {

  def stats: StatisticsManager

  def parameters(params: ParameterManager): Unit
  def problem(problem: String): Unit

  def solution(solution: String): Unit
  def error(e: Throwable): Unit

  def disconnect(status: Try[Result]): Unit
}

sealed trait Result
case object SatFinished extends Result
case object SatUnfinished extends Result
case object Unsat extends Result
