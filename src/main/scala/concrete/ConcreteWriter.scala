package concrete

import cspfj.RESTART
import cspfj.SAT
import cspfj.SolverResult
import cspfj.StatisticsManager
import cspfj.UNKNOWNResult
import cspfj.UNSAT
import cspom.CSPOM

trait ConcreteWriter {
  def solution(solution: Option[Map[String, Int]], concrete: Concrete)
  def write(stats: StatisticsManager)
  def error(e: Throwable)

  def outputFormat(solution: Option[Map[String, Int]], concrete: Concrete) =
    solution match {
      case Some(solution) => concrete.output(solution)
      case None => "UNSAT"
    }

  def disconnect()
}
