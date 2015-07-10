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
import cspom.flatzinc.FZAnnotation
import cspom.flatzinc.FZExpr
import cspom.variable.CSPOMExpression
import cspom.flatzinc.FlatZincParser
import cspom.compiler.CSPOMCompiler
import concrete.generator.cspompatterns.FZPatterns
import concrete.ParameterManager

object FZConcrete extends CSPOMRunner {

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

  def parseGoal(goal: FZSolve, freeSearch: Boolean, pm: ParameterManager): ParameterManager = {
    goal.ann.foreach {
      case a if a.predAnnId == "int_search" || a.predAnnId == "bool_search" =>
        if (!freeSearch) {
          val Seq(_, vca, aa, strategyannotation) = a.expr

          val FZAnnotation(varchoiceannotation, _) = vca

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

          val FZAnnotation(assignmentannotation, _) = aa

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
    pm
  }

  override def loadCSPOM(args: List[String], opt: Map[Symbol, Any]) = {
    val List(fn) = args
    file = CSPOM.file2url(fn.replace(" ", "%20"))

    val (tryLoad, time) = StatisticsManager.timeTry(CSPOM.load(file, FlatZincParser))

    parseTime = time

    tryLoad.map {
      case (cspom, data) =>

        outputVars = cspom.expressionsWithNames.collect {
          case (n, e) if cspom.getAnnotations(n).getSeqParam[FZAnnotation]("fzAnnotations").exists {
            case FZAnnotation("output_var", Seq()) => true
            case _                                 => false
          } => n
        }.toSeq

        outputArrays = cspom.expressionsWithNames.toMap.flatMap {
          case (n, e) =>
            cspom.getAnnotations(n).getSeqParam[FZAnnotation]("fzAnnotations").collectFirst {
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
        parseGoal(goal, opt.contains('free), pm)

        cspom
    }
      .flatMap { cspom =>
        CSPOMCompiler.compile(cspom, FZPatterns())
      }
  }

  def parseSearchMode(goal: FZSolve, solver: CSPOMSolver, freeSearch: Boolean): Unit = {
    val cspom = solver.cspom
    goal.mode match {
      case Satisfy =>
      case Maximize(expr) =>
        val e = expr.toCSPOM(cspom.expressionsWithNames.toMap)
        solver.maximize(cspom.namesOf(e).head)
      case Minimize(expr) =>
        val e = expr.toCSPOM(cspom.expressionsWithNames.toMap)
        solver.minimize(cspom.namesOf(e).head)
      case _ => throw new InvalidParameterException("Cannot execute goal " + goal)
    }

    goal.ann.foreach {
      case a if a.predAnnId == "int_search" || a.predAnnId == "bool_search" =>
        if (!freeSearch) {
          val Seq(e, _, _, _) = a.expr
          val CSPOMSeq(pool) = ann2expr(cspom, e)
          solver.decisionVariables(pool)
        }

      case a => logger.warn(s"Unsupported annotation $a")
    }
  }

  override def applyParametersCSPOM(solver: CSPOMSolver, opt: Map[Symbol, Any]): Unit = {
    parseSearchMode(goal, solver, opt.contains('free))
  }

  private def ann2expr(cspom: CSPOM, e: FZExpr[_]): CSPOMExpression[_] = e match {
    case FZAnnotation(vars, Seq()) => cspom.expression(vars).get

    case FZArrayExpr(list)         => CSPOMSeq(list.map(ann2expr(cspom, _)): _*)

    case e                         => throw new InvalidParameterException("Cannot read search variable list in " + e)
  }

  def description(args: List[String]) =
    args match {
      case List(fileName) => fileName
      case _              => throw new IllegalArgumentException(args.toString)
    }

  private def flattenArrayExpr(ranges: Seq[Seq[Int]], name: String, solution: Map[String, Int]): Seq[Int] = {
    if (ranges.isEmpty) {
      Seq(solution(name))
    } else {
      ranges.head.flatMap(
        i => flattenArrayExpr(ranges.tail, s"$name[$i]", solution))
    }
  }

  private def flattenedSize(ranges: Seq[Seq[Int]]): Int =
    if (ranges.isEmpty) {
      1
    } else {
      ranges.head.size * flattenedSize(ranges.tail)
    }

  //  private def getConstant(n: String): Option[Any] = {
  //    cspom.expression(n).collect {
  //      case CSPOMConstant(v) => v
  //    }
  //  }

  //  def constantOrErr(solution: Map[Variable, Any]): PartialFunction[String, Any] = {
  //    case n: String => getConstant(n).getOrElse(throw new MatchError(s"could not find $n in $solution or $cspom"))
  //  }

  override def outputCSPOM(sol: Map[String, Any]) = {
    val out: Iterable[String] = outputVars.map {
      n => s"$n = ${sol(n)} ;"
    } ++ outputArrays.map {
      case (n, ranges) =>
        val seq =
          cspom.expression(n).getOrElse(throw new MatchError(s"could not find $n in $cspom"))

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

  def main(args: Array[String]): Unit = {
    val status = run(args)

    sys.exit(
      if (status.isSuccess) 0 else 1)
  }

}

