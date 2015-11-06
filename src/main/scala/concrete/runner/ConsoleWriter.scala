package concrete.runner

import scala.xml.NodeSeq
import cspom.StatisticsManager
import scala.util.Try
import scala.util.Success
import scala.util.Failure

class ConsoleWriter(val opts: Map[Symbol, Any], val stats: StatisticsManager) extends ConcreteWriter {

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
    if (opts.contains('stats))
      for ((n, v) <- stats.digest.toSeq.sortBy(_._1)) {
        Console.println(s"% $n = $v")
      }
    Console.println(sol)
  }

  def error(e: Throwable) {
    e.printStackTrace(Console.err)
  }

  private def writeStats(): Unit = {
    if (opts.contains('stats))
      for ((n, v) <- stats.digest.toSeq.sortBy(_._1)) {
        Console.println(s"% $n = $v")
      }
  }

  def disconnect(status: Try[Boolean]) {
    status match {
      case Success(true) => Console.println("==========")
      case Success(false) =>
        writeStats()
        Console.println("=====UNSATISFIABLE=====")
      case Failure(_) =>
        writeStats()
        Console.println("=====UNKNOWN=====")
    }
  }

}
