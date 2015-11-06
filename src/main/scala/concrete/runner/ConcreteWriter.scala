package concrete.runner
import cspom.StatisticsManager
import scala.xml.NodeSeq
import concrete.Variable
import scala.util.Try

trait ConcreteWriter {

  def stats: StatisticsManager

  def parameters(params: NodeSeq): Unit
  def problem(problem: String): Unit

  def solution(solution: String): Unit
  def error(e: Throwable): Unit

  def disconnect(status: Try[Boolean]): Unit
}
