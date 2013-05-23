package concrete.runner

import cspfj.StatisticsManager

class ConsoleWriter extends ConcreteWriter {

  def parameters(params: String) {
    Console.println(params)
  }
  
  def problem(problem: String) {
    Console.println(problem)
  }

  def solution(solution: Option[Map[String, Int]], concrete: ConcreteRunner) {
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
