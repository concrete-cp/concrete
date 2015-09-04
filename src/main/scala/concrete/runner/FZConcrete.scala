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
import cspom.flatzinc.FZArrayIdx
import CSPOM._
import cspom.variable.CSPOMVariable
import cspom.flatzinc.FZArray
import com.typesafe.scalalogging.LazyLogging
import org.scalameter.Quantity

object FZConcrete extends CSPOMRunner with LazyLogging {

  var file: URL = _

  var outputVars: Seq[String] = _

  var outputArrays: Map[String, Seq[Seq[Int]]] = _

  var goal: FZSolve = _

  @Statistic
  var parseTime: Quantity[Double] = _

  override def options(args: List[String], o: Map[Symbol, Any] = Map.empty, realArgs: List[String]): (Map[Symbol, Any], List[String]) = {
    args match {
      case "-f" :: tail           => options(tail, o + ('free -> Unit), realArgs)
      case "-p" :: option :: tail => options(tail, o + ('par -> option.toInt), realArgs)
      case e                      => super.options(e, o, realArgs)
    }
  }

  private def parseGoalAnnotation(ann: FZAnnotation, variables: Map[CSPOMVariable[_], Variable]): Option[Heuristic] = {
    PartialFunction.condOpt(ann) {
      case a if a.predAnnId == "seq_search" =>
        val Seq(strategies: FZArrayExpr[_]) = a.expr
        new SeqHeuristic(
          strategies.value
            .flatMap {
              case a: FZAnnotation => parseGoalAnnotation(a, variables)
              case a               => sys.error(s"Annotation expected in $strategies, found $a")
            }
            .toList)
      case a if a.predAnnId == "int_search" || a.predAnnId == "bool_search" =>
        val Seq(p, vca, aa, strategyannotation) = a.expr
        val CSPOMSeq(pool) = ann2expr(cspom, p)
        val decisionVariables = pool.collect {
          case v: CSPOMVariable[_] => variables(v)
        }
          .toList

        val FZAnnotation(varchoiceannotation, _) = vca

        val varh: VariableHeuristic = varchoiceannotation match {
          case "input_order"      => new LexVar(pm, decisionVariables)
          case "first_fail"       => new Dom(pm, decisionVariables)
          case "antifirst_fail"   => new MaxDom(pm, decisionVariables)
          case "smallest"         => new SmallestValue(pm, decisionVariables)
          case "largest"          => new LargestValue(pm, decisionVariables)
          case "occurrence"       => new DDeg(pm, decisionVariables)
          case "most_constrained" => new Brelaz(pm, decisionVariables)
          case "max_regret"       => new MaxRegret(pm, decisionVariables)
          case h =>
            logger.warn(s"Unsupported varchoice $h")
            CrossHeuristic.defaultVar(pm, decisionVariables)

        }

        val FZAnnotation(assignmentannotation, _) = aa

        val valh = assignmentannotation match {
          case "indomain"               => new Lexico()
          case "indomain_min"           => new Lexico()
          case "indomain_max"           => new RevLexico()
          case "indomain_median"        => new MedValue()
          case "indomain_random"        => new RandomValue()
          case "indomain_split"         => new Split()
          case "indomain_reverse_split" => new RevSplit()
          case "indomain_interval"      => new IntervalBranch()
          case h =>
            logger.warn(s"Unsupported assignment heuristic $h")
            CrossHeuristic.defaultVal(pm)
        }

        new CrossHeuristic(pm, varh, valh)
    }
  }

  def parseGoal(goal: FZSolve, freeSearch: Boolean, variables: Map[CSPOMVariable[_], Variable]): Seq[Heuristic] = {
    if (freeSearch) {
      Seq()
    } else {
      goal.ann.flatMap(parseGoalAnnotation(_, variables))
    }
  }

  override def loadCSPOM(args: List[String], opt: Map[Symbol, Any]) = {
    val List(fn) = args
    file = CSPOM.file2url(fn.replace(" ", "%20"))

    val (tryLoad, time) = StatisticsManager.measureTry(CSPOM.load(file, FlatZincParser))

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

  }

  override def applyParametersPre(opt: Map[Symbol, Any]): Unit = {
    val heuristics = parseGoal(goal, opt.contains('free), variables)
    val decisionVariables = heuristics
      .collect {
        case c: CrossHeuristic => c.variableHeuristic.decisionVariables
      }
      .flatten.distinct

    val heuristic =
      if (decisionVariables.size < variables.size) {
        new SeqHeuristic(heuristics.toList :+ CrossHeuristic(pm, variables.values.toList))
      } else {
        heuristics match {
          case Seq(h) => h
          case m      => new SeqHeuristic(m.toList)
        }
      }

    logger.info(heuristic.toString + ", should restart: " + heuristic.shouldRestart)
    // println(heuristic)
    pm("mac.heuristic") = heuristic

  }

  override def applyParametersCSPOM(solver: CSPOMSolver, opt: Map[Symbol, Any]): Unit = {
    parseSearchMode(goal, solver, opt.contains('free))
  }

  private def ann2expr(cspom: CSPOM, e: FZExpr[_]): CSPOMExpression[_] = e match {
    case FZAnnotation(vars, Seq()) => cspom.expression(vars).get

    case FZArrayExpr(list)         => list.map(ann2expr(cspom, _))

    case FZArrayIdx(array, idx) => cspom.expression(array)
      .collect {
        case s: CSPOMSeq[_] => s(idx)
      }
      .get

    case e => throw new InvalidParameterException("Cannot read search variable list in " + e)
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

