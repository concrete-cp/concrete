package concrete.runner

import scala.util.Failure
import scala.util.Success
import scala.util.Try
import scala.xml.NodeSeq

import cspom.StatisticsManager

class ConsoleWriter(val opts: Map[Symbol, Any], val stats: StatisticsManager) extends ConcreteWriter {

  def parameters(params: NodeSeq,  it: Int) {
    for (p <- params \\ "p") {

      Console.println(s"% ${(p \ "@name").text} = ${p.text}")

    }
    Console.println(s"% iteration $it")

  }

  def problem(problem: String) {
    if (opts.contains('stats))
      Console.println(s"% $problem")
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
    writeStats()
    status match {
      case Success(true) => Console.println("==========")
      case Success(false) =>
        Console.println("=====UNSATISFIABLE=====")
      case Failure(_) =>
        Console.println("=====UNKNOWN=====")
    }
  }

}
