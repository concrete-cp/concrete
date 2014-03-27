package concrete.runner

import cspom.StatisticsManager
import scala.xml.NodeSeq
import concrete.Variable

class ConsoleWriter extends ConcreteWriter {

  var unsat = false

  def parameters(params: NodeSeq) {
    for (p <- params \\ "p") {

      Console.println(s"% ${p \ "@name" text} = ${p.text}")

    }

  }

  def problem(problem: String) {
    Console.println("% " + problem)
  }

  def solution(solution: Option[Map[Variable, Any]], concrete: ConcreteRunner) {
    solution match {
      case Some(solution) => Console.println(concrete.output(solution))
      case None => unsat = true
    }
  }

  def write(stats: StatisticsManager) {
    for ((n, v) <- stats.digest.toSeq.sortBy(_._1)) {
      Console.println(s"% $n = $v")
    }

  }

  def error(e: Throwable) {
    e.printStackTrace(Console.out)
    //    Console.println(e)
    //    Console.println(e.getStackTrace().mkString("\n"))
  }

  def disconnect() {
    if (unsat) {
      Console.println("=====UNSATISFIABLE=====")
    } else {
      Console.println("==========")
    }
  }

}
