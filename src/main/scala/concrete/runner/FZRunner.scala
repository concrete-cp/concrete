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
import cspom.flatzinc.FZAnnotation
import cspom.flatzinc.FZArrayExpr
import cspom.flatzinc.FZSetConst
import cspom.variable.CSPOMSeq
import cspom.variable.CSPOMConstant

object FZConcrete extends ConcreteRunner with App {

  var file: URL = _

  var data: Map[Symbol, Any] = _

  override def loadCSPOM(args: List[String]) = {
    val List(fn) = args

    val f = new URI(fn)

    file = if (f.getScheme() == null) {
      new URL("file:" + f)
    } else {
      println(f.getScheme())
      f.toURL
    }

    //println(file)

    val cspom = CSPOM.load(file)
    data = cspom._2
    //println(cspom._1)
    cspom._1
  }

  def description(args: List[String]) =
    args match {
      case List(fileName) => fileName
      case _ => throw new IllegalArgumentException(args.toString)
    }

  def flattenArrayExpr(ranges: Seq[Seq[Int]], name: String, solution: Map[String, Int]): Seq[Int] = {
    if (ranges.isEmpty) {
      Seq(solution(name))
    } else {
      ranges.head.flatMap(
        i => flattenArrayExpr(ranges.tail, s"$name[$i]", solution))
    }
  }

  def flattenedSize(ranges: Seq[Seq[Int]]): Int =
    if (ranges.isEmpty) {
      1
    } else {
      ranges.head.size * flattenedSize(ranges.tail)
    }

  def getConstant(n: String): Option[Any] = {
    cProblem.namedExpressions.get(n).collect {
      case CSPOMConstant(v) => v
    }
  }

  override def output(solution: Map[String, Any]) = {
    val sol1 = solution.flatMap {
      case (s, a) => s.split("\\|\\|").map(_ -> a)
    }

    val sol = sol1.orElse(Function.unlift[String, Any](getConstant(_)))

    val out: Iterable[String] = cProblem.namedExpressions.flatMap {
      case (name, output) =>
        output.getParam("fzAnnotations", classOf[Seq[FZAnnotation]]).flatMap(_.collectFirst {

          case FZAnnotation("output_var", Seq()) =>
            name + " = " + sol(name) + ";"
          case FZAnnotation("output_array", Seq(data: FZArrayExpr[Seq[Int]])) =>
            val FZArrayExpr(array) = data
            val ranges = array.map {
              case FZSetConst(range) => range
            }
            val CSPOMSeq(variables, initRange, _) = output
            require(initRange.size == flattenedSize(ranges))
            val solutions = initRange.map(i => sol(s"$name[$i]"))

            s"$name = array${array.size}d(${
              ranges.map(range => s"${range.head}..${range.last}, ").mkString
            }${solutions.mkString("[", ", ", "]")});"

        })
      //      case (name, output) if output.params("output_array") =>
      //        name + " : " + output.params

    }

    out.mkString("\n") + "\n----------\n"
    //flatSolution(solution, variables).mkString(" ")
  }

  def control(solution: Map[String, Any]) = ???

  run(args)

}

