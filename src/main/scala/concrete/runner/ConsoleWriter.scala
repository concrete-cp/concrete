package concrete.runner

import cspom.StatisticsManager
import scala.xml.NodeSeq
import concrete.Variable
import concrete.UNSATException

class ConsoleWriter extends ConcreteWriter {

  var sat = false
  var error = false

  def parameters(params: NodeSeq) {
    for (p <- params \\ "p") {

      Console.println(s"% ${(p \ "@name").text} = ${p.text}")

    }

  }

  def problem(problem: String) {
    Console.println("% " + problem)
  }

  def solution(sol: String) {
    sat = true
    Console.println(sol)
  }

  def write(stats: StatisticsManager) {
    for ((n, v) <- stats.digest.toSeq.sortBy(_._1)) {
      Console.println(s"% $n = $v")
    }

  }

  def error(e: Throwable) {
    error |= (!e.isInstanceOf[UNSATException])
    e.printStackTrace(Console.err)
    //    Console.println(e)
    //    Console.println(e.getStackTrace().mkString("\n"))
  }

  def disconnect() {
    if (error) {
      Console.println("=====UNKNOWN=====")
    } else if (sat) {
      Console.println("==========")
    } else {
      Console.println("=====UNSATISFIABLE=====")
    }
  }

}
