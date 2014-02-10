package concrete.runner

import cspom.StatisticsManager
import scala.xml.NodeSeq

class ConsoleWriter extends ConcreteWriter {

  def parameters(params: NodeSeq) {
    for (p <- params \\ "p") {

      Console.println(s"# ${p \ "@name" text} = ${p.text}")

    }

  }

  def problem(problem: String) {
    Console.println("# " + problem)
  }

  def solution(solution: Option[Map[String, Int]], concrete: ConcreteRunner) {
    Console.println(outputFormat(solution, concrete))
  }

  def write(stats: StatisticsManager) {
    for ((n, v) <- stats.digest.toSeq.sortBy(_._1)) {
      Console.println(s"# $n = $v")
    }

  }

  def error(e: Throwable) {
    e.printStackTrace(Console.out)
    //    Console.println(e)
    //    Console.println(e.getStackTrace().mkString("\n"))
  }

  def disconnect() {
    //Console.println("==========")
  }

}
