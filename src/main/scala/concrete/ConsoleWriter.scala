package concrete

import cspfj.StatisticsManager
import cspom.CSPOM
import cspfj.SolverResult

class ConsoleWriter(params: String) extends ConcreteWriter {

  println(params)

  def solution(solution: SolverResult, problem: CSPOM) {
    Console.println(outputFormat(solution, problem))
  }

  def write(stats: StatisticsManager) {
    Console.println(stats)
  }
  
  def error(e: Throwable) {
    Console.println(e)
    Console.println(e.getStackTrace().mkString("\n"))
  }
  
  def disconnect() {
    Console.println("End.")
  }

}