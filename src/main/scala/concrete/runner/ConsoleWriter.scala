package concrete.runner

import cspom.StatisticsManager
import scala.xml.NodeSeq
import concrete.Variable
import concrete.UNSATException

class ConsoleWriter extends ConcreteWriter {

  def parameters(params: NodeSeq) {
    for (p <- params \\ "p") {

      Console.println(s"% ${(p \ "@name").text} = ${p.text}")

    }

  }

  def problem(problem: String) {
    Console.println("% " + problem)
  }

  def solution(sol: String) {
    Console.println(sol)
  }

  def write(stats: StatisticsManager) {
    for ((n, v) <- stats.digest.toSeq.sortBy(_._1)) {
      Console.println(s"% $n = $v")
    }

  }

  def error(e: Throwable) {
    e.printStackTrace(Console.err)
  }

  def disconnect(status: RunnerStatus) {
    status match {
      case Sat => Console.println("==========")
      case Unsat => Console.println("=====UNSATISFIABLE=====")
      case _ => Console.println("=====UNKNOWN=====")
    }
  }

}
