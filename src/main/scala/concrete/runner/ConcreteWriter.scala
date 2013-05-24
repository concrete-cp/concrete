package concrete.runner
import concrete.StatisticsManager

trait ConcreteWriter {

  def parameters(params: String)
  def problem(problem: String)

  def solution(solution: Option[Map[String, Int]], concrete: ConcreteRunner)
  def write(stats: StatisticsManager)
  def error(e: Throwable)

  def outputFormat(solution: Option[Map[String, Int]], concrete: ConcreteRunner) =
    solution match {
      case Some(solution) => concrete.output(solution)
      case None => "UNSAT"
    }

  def disconnect()
}
