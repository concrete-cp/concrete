package concrete.runner

import scala.xml.NodeSeq
import cspom.StatisticsManager
import scala.util.Try
import scala.util.Success
import scala.util.Failure

class ConsoleWriter(opts: Map[Symbol, Any]) extends ConcreteWriter {

  def parameters(params: NodeSeq) {
    for (p <- params \\ "p") {

      Console.println(s"% ${(p \ "@name").text} = ${p.text}")

    }

  }

  def problem(problem: String) {
    if (opts.contains('stats))
      Console.println("% " + problem)
  }

  def solution(sol: String) {
    Console.println(sol)
  }

  def write(stats: StatisticsManager) {
    if (opts.contains('stats))
      for ((n, v) <- stats.digest.toSeq.sortBy(_._1)) {
        Console.println(s"% $n = $v")
      }

  }

  def error(e: Throwable) {
    e.printStackTrace(Console.err)
  }

  def disconnect(status: Try[Boolean]) {
    status match {
      case Success(true)  => Console.println("==========")
      case Success(false) => Console.println("=====UNSATISFIABLE=====")
      case Failure(_)     => Console.println("=====UNKNOWN=====")
    }
  }

}
