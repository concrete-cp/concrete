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
import cspom.flatzinc.FZSolve
import cspom.flatzinc.Satisfy
import cspom.flatzinc.Maximize
import cspom.flatzinc.FZVarParId
import cspom.flatzinc.Minimize
import java.security.InvalidParameterException
import concrete.Variable
import concrete.Solver
import cspom.variable.CSPOMExpression

object FZConcrete extends ConcreteRunner with App {

  var file: URL = _

  var outputVars: Seq[String] = _

  var outputArrays: Map[String, Seq[Seq[Int]]] = _

  var goal: FZSolve = _

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

    val (cspom, data) = CSPOM.load(file)

    outputVars = cspom.namedExpressions.collect {
      case (n, e) if e.getSeqParam("fzAnnotations", classOf[FZAnnotation]).exists {
        case FZAnnotation("output_var", Seq()) => true
        case _ => false
      } => n
    }.toSeq

    outputArrays = cspom.namedExpressions.flatMap {
      case (n, e) =>
        e.getSeqParam("fzAnnotations", classOf[FZAnnotation]).collectFirst {
          case FZAnnotation("output_array", Seq(data: FZArrayExpr[_])) =>
            val FZArrayExpr(array) = data
            val ranges = array.map {
              case FZSetConst(range) => range
              case _ => throw new InvalidParameterException("An array of set constants is expected here: " + data)
            }
            n -> ranges
        }
    }

    val Some(goal: FZSolve) = data.get('goal)

    this.goal = goal

    //println(cspom._1)
    cspom
  }

  override def applyParameters(solver: Solver, variables: Map[CSPOMVariable[_], Variable]) = {

    goal.mode match {
      case Satisfy =>
      case Maximize(expr) =>
        val e = expr.toCSPOM(_cProblem.get.namedExpressions).asInstanceOf[CSPOMVariable[_]]
        solver.maximize(variables(e))
      case Minimize(expr) =>
        val e = expr.toCSPOM(_cProblem.get.namedExpressions).asInstanceOf[CSPOMVariable[_]]
        solver.minimize(variables(e))
      case _ => throw new InvalidParameterException("Cannot execute goal " + goal)
    }
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

  private def getConstant(n: String): Option[Any] = {
    cProblem.expression(n).collect {
      case CSPOMConstant(v) => v
    }
  }

  def constantOrErr(solution: Map[Variable, Any]): PartialFunction[String, Any] = {
    case n: String => getConstant(n).getOrElse(throw new MatchError(s"could not find $n in $solution or $cProblem"))
  }

  override def output(solution: Map[Variable, Any]) = {
    val sol1 = solution.flatMap {
      case (s, a) => s.name.split("\\|\\|").map(_ -> a)
    }

    val sol = sol1.orElse(constantOrErr(solution))

    val out: Iterable[String] = outputVars.map {
      n => s"$n = ${sol(n)} ;"
    } ++ outputArrays.map {
      case (n, ranges) =>
        val CSPOMSeq(variables, initRange, _) =
          cProblem.namedExpressions.getOrElse(n, throw new MatchError(s"could not find $n in $solution or $cProblem"))
        require(initRange.size == flattenedSize(ranges))
        val solutions = initRange.map(i => sol(s"$n[$i]"))

        s"$n = array${ranges.size}d(${
          ranges.map(range => s"${range.head}..${range.last}, ").mkString
        }${solutions.mkString("[", ", ", "]")});"

    }

    out.mkString("\n") + "\n----------\n"
    //flatSolution(solution, variables).mkString(" ")
  }

  def control(solution: Map[Variable, Any]) = ???

  run(args)

}

