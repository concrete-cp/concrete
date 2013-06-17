package concrete.runner

import concrete.StatisticsManager
import scala.xml.NodeSeq

class ConsoleWriter extends ConcreteWriter {

  def parameters(params: NodeSeq) {
    val p = for (p <- params \\ "p") yield {
      //Console.println(p)
      Console.println(s"${p \ "@name" text} = ${p.text}")

    }

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
