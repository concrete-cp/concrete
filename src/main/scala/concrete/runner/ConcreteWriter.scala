package concrete.runner
import cspom.StatisticsManager
import scala.xml.NodeSeq
import concrete.Variable

trait ConcreteWriter {

  def parameters(params: NodeSeq): Unit
  def problem(problem: String): Unit

  def solution(solution: String): Unit
  def write(stats: StatisticsManager): Unit
  def error(e: Throwable): Unit

  def disconnect(status: RunnerStatus): Unit
}
