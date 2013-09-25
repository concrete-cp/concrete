package concrete.runner
import java.net.URL
import cspom.CSPOM

object XCSPConcrete extends ConcreteRunner with App {

  override def loadCSPOM(args: List[String]) = {
    val List(fileName) = args
    val file = new URL(fileName)
    CSPOM.load(file)
  }

  def description(args: List[String]) =
    args match {
      case List(fileName) => fileName
      case _ => throw new IllegalArgumentException(args.toString)
    }

  def output(solution: Map[String, Int]) = {
    cProblem.variables.filter(!_.params("var_is_introduced")).map(v =>
      solution(v.name)).mkString(" ")
  }

  def control(solution: Map[String, Int]) = {
    cProblem.controlInt(solution) match {
      case s: Set[_] if s.isEmpty => None
      case s: Set[_] => Some(s.mkString(", "))
    }
  }

  run(args)

}
