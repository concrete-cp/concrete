package concrete.runner

import concrete.ParameterManager
import cspom.StatisticsManager

class FZWriter(val opts: Map[Symbol, Any], val stats: StatisticsManager) extends ConcreteWriter {

  def parameters(params: ParameterManager) {
    for ((k, v) <- params.parameters) {
      Console.println(s"% $k = $v")
    }
  }

  def problem(problem: String) {
    if (opts.contains('stats))
      Console.println(s"% $problem")
  }

  def printSolution(sol: String, obj: Option[Any]) {
    if (opts.contains('all)) writeStats()
    Console.println(sol)
    for (o <- obj) {
      Console.println("% objective = " + o)
    }
  }

  def error(e: Throwable) {
    // e.printStackTrace(Console.err)
  }

  def disconnect(status: Result) {
    writeStats()
    Console.println {
      status match {
        case FullExplore if lastSolution.isDefined => "=========="
        case FullExplore => "=====UNSATISFIABLE====="
        case Unfinished(_) if lastSolution.isDefined => ""
        case _ => "=====UNKNOWN====="
      }
    }
  }

  private def writeStats(): Unit = {
    if (opts.contains('stats))
      for ((n, v) <- stats.digest.toSeq.sortBy(_._1)) {
        Console.println(s"% $n = $v")
      }
  }

}
