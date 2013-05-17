package concrete

import cspfj.StatisticsManager
import cspom.CSPOM
import cspfj.SolverResult

class ConsoleWriter(params: String) extends ConcreteWriter {

  Console.println(params)

  def solution(solution: Option[Map[String, Int]], concrete: Concrete) {
    Console.println(outputFormat(solution, concrete))
  }

  def write(stats: StatisticsManager) {
    Console.println(stats)
  }

  def error(e: Throwable) {
    e.printStackTrace(Console.out)
    //    Console.println(e)
    //    Console.println(e.getStackTrace().mkString("\n"))
  }

  def disconnect() {
    Console.println("End.")
  }

}
