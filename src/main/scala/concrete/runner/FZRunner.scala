package concrete.runner
import java.net.URL
import cspom.CSPOM
import abscon.instance.tools.InstanceParser
import java.util.StringTokenizer
import java.io.File
import abscon.instance.components.PConstraint
import abscon.instance.components.PVariable
import scala.collection.JavaConversions
import java.net.URI
import cspom.variable.CSPOMVariable

object FZConcrete extends ConcreteRunner with App {

  var file: URL = _

  var data: Map[Symbol, Any] = _

  override def loadCSPOM(args: List[String]) = {
    val List(fn) = args

    val f = new URI(fn)

    file = if (f.getScheme() == null) {
      new URL("file://" + f)
    } else {
      f.toURL
    }

    val cspom = CSPOM.load(file)
    data = cspom._2
    cspom._1
  }

  def description(args: List[String]) =
    args match {
      case List(fileName) => fileName
      case _ => throw new IllegalArgumentException(args.toString)
    }

  override def output(solution: Map[String, Int]) = {

    val out = cProblem.namedExpressions.collect {
      case (name, output) if output.params("output_var") =>
        name + " = " + solution(name) + ";"

      case (name, output) if output.params.exists(_.contains("output_array")) =>
        name + " : " + output.params
        
    }

    out.mkString("\n")
    //flatSolution(solution, variables).mkString(" ")
  }

  def control(solution: Map[String, Int]) = ???

  run(args)

}

