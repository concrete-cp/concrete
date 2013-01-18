package concrete

import cspfj.RESTART
import cspfj.SAT
import cspfj.SolverResult
import cspfj.StatisticsManager
import cspfj.UNKNOWNResult
import cspfj.UNSAT
import cspom.CSPOM

trait ConcreteWriter {
  def solution(solution: SolverResult, concrete: Concrete)
  def write(stats: StatisticsManager)
  def error(e: Throwable)

  def outputFormat(solution: SolverResult, concrete: Concrete) =
    solution match {
      case SAT(solution) => concrete.output(solution)
      case UNSAT => "UNSAT"
      case UNKNOWNResult => "UNKNOWN"
      case RESTART => throw new IllegalStateException
    }

  def disconnect()
}
