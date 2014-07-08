package concrete.runner

import java.net.URI
import java.net.URL
import java.security.InvalidParameterException

import concrete.CSPOMSolver
import concrete.Variable
import cspom.CSPOM
import cspom.flatzinc.FZAnnotation
import cspom.flatzinc.FZArrayExpr
import cspom.flatzinc.FZSetConst
import cspom.flatzinc.FZSolve
import cspom.flatzinc.Maximize
import cspom.flatzinc.Minimize
import cspom.flatzinc.Satisfy
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMSeq

object FZConcrete extends CSPOMRunner with App {

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

    println(cspom)
    cspom
  }

  override def applyParametersCSPOM(solver: CSPOMSolver) = {
    goal.mode match {
      case Satisfy =>
      case Maximize(expr) =>
        val e = expr.toCSPOM(cspom.namedExpressions)
        solver.maximize(cspom.namesOf(e).head)
      case Minimize(expr) =>
        val e = expr.toCSPOM(cspom.namedExpressions)
        solver.minimize(cspom.namesOf(e).head)
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
    cspom.expression(n).collect {
      case CSPOMConstant(v) => v
    }
  }

  def constantOrErr(solution: Map[Variable, Any]): PartialFunction[String, Any] = {
    case n: String => getConstant(n).getOrElse(throw new MatchError(s"could not find $n in $solution or $cspom"))
  }

  override def outputCSPOM(solution: Option[Map[String, Any]]) =
    solution.map { sol =>
      val out: Iterable[String] = outputVars.map {
        n => s"$n = ${sol(n)} ;"
      } ++ outputArrays.map {
        case (n, ranges) =>
          val CSPOMSeq(_, initRange, _) =
            cspom.namedExpressions.getOrElse(n, throw new MatchError(s"could not find $n in $cspom"))
          require(initRange.size == flattenedSize(ranges))
          val solutions = initRange.map(i => sol(s"$n[$i]"))

          s"$n = array${ranges.size}d(${
            ranges.map(range => s"${range.head}..${range.last}, ").mkString
          }${solutions.mkString("[", ", ", "]")});"

      }

      out.mkString("\n") + "\n----------\n"
      //flatSolution(solution, variables).mkString(" ")
    } getOrElse {
      "\n=====UNSATISFIABLE=====\n"
    }

  def controlCSPOM(solution: Map[String, Any]) = ???

  run(args)

}

