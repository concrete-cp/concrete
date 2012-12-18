package concrete

import cspfj.RESTART
import cspfj.SAT
import cspfj.SolverResult
import cspfj.StatisticsManager
import cspfj.UNKNOWNResult
import cspfj.UNSAT
import cspom.CSPOM

trait ConcreteWriter {
  def solution(solution: SolverResult, problem: CSPOM)
  def write(stats: StatisticsManager)
  def error(e: Throwable)

  def outputFormat(solution: SolverResult, problem: CSPOM) =
    solution match {
      case SAT(solution) =>
        problem.variables.filter(!_.auxiliary).map(v =>
          solution.getOrElse(v.name, v.domain.values.head)).mkString(" ")
      case UNSAT => "UNSAT"
      case UNKNOWNResult => "UNKNOWN"
      case RESTART => throw new IllegalStateException
    }
  
  def disconnect()
}
