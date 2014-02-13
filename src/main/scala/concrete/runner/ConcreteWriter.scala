package concrete.runner
import cspom.StatisticsManager
import scala.xml.NodeSeq

trait ConcreteWriter {

  def parameters(params: NodeSeq)
  def problem(problem: String)

  def solution(solution: Option[Map[String, Any]], concrete: ConcreteRunner)
  def write(stats: StatisticsManager)
  def error(e: Throwable)

  def disconnect()
}
