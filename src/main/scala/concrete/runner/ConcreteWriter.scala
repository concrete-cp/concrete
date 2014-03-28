package concrete.runner
import cspom.StatisticsManager
import scala.xml.NodeSeq
import concrete.Variable

trait ConcreteWriter {

  def parameters(params: NodeSeq)
  def problem(problem: String)

  def solution(solution: String)
  def write(stats: StatisticsManager)
  def error(e: Throwable)

  def disconnect()
}
