package concrete.runner

import concrete.ParameterManager
import cspom.StatisticsManager

class XCSP3Writer(val opts: Map[Symbol, Any], val stats: StatisticsManager) extends ConcreteWriter {

  Console.println(s"c Concrete v3.4 running")

  def parameters(params: ParameterManager) {
    for ((k, v) <- params.parameters) {
      Console.println(s"c $k = $v")
    }
  }

  def problem(problem: String) {
    if (opts.contains('stats)) {
      for (l <- problem.toString.split("\n")) {
        Console.println("c " + l)
      }
    }
  }

  def printSolution(sol: String, obj: Option[Any]) {
    writeStats()
    for (o <- obj) {
      Console.println(s"o $o")
    }
  }

  private def writeStats(): Unit = {
    if (opts.contains('stats))
      for ((n, v) <- stats.digest.toSeq.sortBy(_._1)) {
        Console.println(s"c $n = $v")
      }
  }

  def error(e: Throwable) {
    e.printStackTrace(Console.err)
  }

  def disconnect(status: Result) {
    writeStats()
    for (s <- lastSolution) {
      Console.println(s.split("\n").map("v " + _).mkString("\n"))
    }
    Console.println {
      status match {
        case FullExplore if lastSolution.isDefined => "s OPTIMUM FOUND"
        case FullExplore => "s UNSATISFIABLE"
        case Unfinished(_) if lastSolution.isDefined => "s SATISFIABLE"
        case Unfinished(Some(_: UnsupportedOperationException)) => "s UNSUPPORTED"
        case _ => "s UNKNOWN"


      }
    }
  }

}
