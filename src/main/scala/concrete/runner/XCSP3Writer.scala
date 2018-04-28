package concrete.runner

import concrete.ParameterManager
import cspom.StatisticsManager

class XCSP3Writer(val pm:ParameterManager, problem:String, val stats: StatisticsManager) extends ConcreteWriter {

  Console.println(s"c Concrete v$version running")

  if (pm.contains("s")) {
    for (l <- problem.toString.split("\n")) {
      Console.println("c " + l)
    }

    for ((k, v) <- pm.parameters) {
      Console.println(s"c $k = $v")
    }
  }


  def printSolution(sol: String, obj: Seq[(String, Any)]) {
    writeStats()
    for ((_, v) <- obj) {
      Console.println(s"o $v")
    }
  }

  private def writeStats(): Unit = {
    if (pm.contains("s"))
      for ((n, v) <- stats.digest.toSeq.sortBy(_._1)) {
        Console.println(s"c $n = $v")
      }
  }

  def error(e: Throwable) {
    // e.printStackTrace(Console.err)
  }

  def disconnect(status: RunnerResult) {
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
