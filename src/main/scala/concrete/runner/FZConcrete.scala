package concrete.runner

import java.io.IOException
import java.net.URI
import java.net.URL
import java.security.InvalidParameterException
import scala.reflect.runtime.universe
import concrete.CSPOMSolver
import concrete.Variable
import cspom.CSPOM
import cspom.CSPParseException
import cspom.Statistic
import cspom.StatisticsManager
import cspom.flatzinc.FZAnnotation
import cspom.flatzinc.FZArrayExpr
import cspom.flatzinc.FZSetConst
import cspom.flatzinc.FZSolve
import cspom.flatzinc.FZVarParId
import cspom.flatzinc.Maximize
import cspom.flatzinc.Minimize
import cspom.flatzinc.Satisfy
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMSeq
import concrete.heuristic._

object FZConcrete extends CSPOMRunner with App {

  var file: URL = _

  var outputVars: Seq[String] = _

  var outputArrays: Map[String, Seq[Seq[Int]]] = _

  var goal: FZSolve = _

  @Statistic
  var parseTime: Double = _

  override def options(args: List[String], o: Map[Symbol, Any] = Map.empty, realArgs: List[String]): (Map[Symbol, Any], List[String]) = {
    args match {
      case "-f" :: tail => options(tail, o + ('free -> Unit), realArgs)
      case e            => super.options(e, o, realArgs)
    }
  }

  override def loadCSPOM(args: List[String], opt: Map[Symbol, Any]) = {
    val List(fn) = args

    val f = URI.create(fn.replace(" ", "%20"))

    file = if (f.getScheme() == null) {
      new URL("file:" + f)
    } else {
      //println(f.getScheme())
      f.toURL
    }

    val ((cspom, data), time) = StatisticsManager.time(CSPOM.load(file))

    parseTime = time

    outputVars = cspom.namedExpressions.collect {
      case (n, e) if e.getSeqParam[FZAnnotation]("fzAnnotations").exists {
        case FZAnnotation("output_var", Seq()) => true
        case _                                 => false
      } => n
    }.toSeq

    outputArrays = cspom.namedExpressions.toMap.flatMap {
      case (n, e) =>
        e.getSeqParam[FZAnnotation]("fzAnnotations").collectFirst {
          case FZAnnotation("output_array", Seq(data: FZArrayExpr[_])) =>
            val FZArrayExpr(array) = data
            val ranges = array.map {
              case FZSetConst(range) => range
              case _                 => throw new InvalidParameterException("An array of set constants is expected here: " + data)
            }
            n -> ranges
        }
    }

    val Some(goal: FZSolve) = data.get('goal)

    this.goal = goal
    goal.ann.foreach {
      case a if a.predAnnId == "int_search" || a.predAnnId == "bool_search" =>
        if (!opt.contains('free)) {
          val Seq(_, FZVarParId(varchoiceannotation), FZVarParId(assignmentannotation), strategyannotation) = a.expr

          varchoiceannotation match {
            case "input_order" =>
              pm("heuristic.variable") = classOf[LexVar]
              pm("mac.restartLevel") = -1
            case "first_fail"       => pm("heuristic.variable") = classOf[Dom]
            case "antifirst_fail"   => pm("heuristic.variable") = classOf[MaxDom]
            // case "smallest"
            // case "largest"
            case "occurrence"       => pm("heuristic.variable") = classOf[DDeg]
            case "most_constrained" => pm("heuristic.variable") = classOf[Brelaz]
            case h                  => logger.warn(s"Unsupported varchoice $h")
          }

          assignmentannotation match {
            case "indomain_min"    => pm("heuristic.value") = classOf[Lexico]
            case "indomain_max"    => pm("heuristic.value") = classOf[RevLexico]
            case "indomain_middle" => pm("heuristic.value") = classOf[MedValue]
            case "indomain_random" => pm("heuristic.value") = classOf[RandomValue]
            case h                 => logger.warn(s"Unsupported assignment $h")
          }
        }

      case a => logger.warn(s"Unsupported annotation $a")
    }

    cspom
  }

  override def applyParametersCSPOM(solver: CSPOMSolver, opt: Map[Symbol, Any]) = {
    goal.mode match {
      case Satisfy =>
      case Maximize(expr) =>
        val e = expr.toCSPOM(cspom.namedExpressions.toMap)
        solver.maximize(cspom.namesOf(e).head)
      case Minimize(expr) =>
        val e = expr.toCSPOM(cspom.namedExpressions.toMap)
        solver.minimize(cspom.namesOf(e).head)
      case _ => throw new InvalidParameterException("Cannot execute goal " + goal)
    }

    goal.ann.foreach {
      case a if a.predAnnId == "int_search" || a.predAnnId == "bool_search" =>
        if (!opt.contains('free)) {
          val Seq(vars, _, _, _) = a.expr

          val CSPOMSeq(pool) = vars.toCSPOM(cspom.namedExpressions.toMap)

          solver.decisionVariables(pool)
        }

      case a => logger.warn(s"Unsupported annotation $a")
    }

  }

  def description(args: List[String]) =
    args match {
      case List(fileName) => fileName
      case _              => throw new IllegalArgumentException(args.toString)
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

  override def outputCSPOM(sol: Map[String, Any]) = {
    val out: Iterable[String] = outputVars.map {
      n => s"$n = ${sol(n)} ;"
    } ++ outputArrays.map {
      case (n, ranges) =>
        val seq =
          cspom.namedExpressions.getOrElse(n, throw new MatchError(s"could not find $n in $cspom"))

        val initRange = seq.asInstanceOf[CSPOMSeq[_]].definedIndices

        require(initRange.size == flattenedSize(ranges))
        val solutions = initRange.map(i => sol(s"$n[$i]"))

        s"$n = array${ranges.size}d(${
          ranges.map(range => s"${range.head}..${range.last}, ").mkString
        }${solutions.mkString("[", ", ", "]")});"

    }
    
    //++ cspom.namedExpressions.keys.flatMap(_.split("\\|\\|")).map {
    //  n => s"$n = ${sol(n)} ;"
    //}

    out.mkString("\n") + "\n----------\n"
    //flatSolution(solution, variables).mkString(" ")
  }

  def controlCSPOM(solution: Map[String, Any]) = ???

  val status = run(args)

  sys.exit(status match {
    case Sat | Unsat => 0
    case _           => 1
  })

}

