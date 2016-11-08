package concrete.runner

import java.net.URL
import java.security.InvalidParameterException

import scala.reflect.runtime.universe

import org.scalameter.Quantity

import com.typesafe.scalalogging.LazyLogging

import concrete.CSPOMSolver
import concrete.Problem
import concrete.Variable
import concrete.generator.cspompatterns.FZPatterns
import concrete.heuristic._
import concrete.heuristic.value._
import concrete.heuristic.variable._
import cspom.CSPOM
import cspom.CSPOM.seq2CSPOMSeq
import cspom.CSPOMGoal
import cspom.Statistic
import cspom.StatisticsManager
import cspom.WithParam
import cspom.compiler.CSPOMCompiler
import cspom.flatzinc.FZAnnotation
import cspom.flatzinc.FZArrayExpr
import cspom.flatzinc.FZArrayIdx
import cspom.flatzinc.FZExpr
import cspom.flatzinc.FZSetConst
import cspom.flatzinc.FlatZincParser
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMSeq
import cspom.variable.CSPOMVariable
import cspom.variable.IntExpression

object FZConcrete extends CSPOMRunner with LazyLogging {

  var file: URL = _

  var outputVars: Seq[String] = _

  var outputArrays: Map[String, Seq[Seq[Int]]] = _

  @Statistic
  var parseTime: Quantity[Double] = _

  override def options(args: List[String], o: Map[Symbol, Any] = Map.empty, realArgs: List[String]): (Map[Symbol, Any], List[String]) = {
    args match {
      case "-f" :: tail => options(tail, o + ('free -> Unit), realArgs)
      case "-ff" :: tail => options(tail, o + ('totalFree -> Unit), realArgs)
      case "-p" :: option :: tail => options(tail, o + ('par -> option.toInt), realArgs)
      case e => super.options(e, o, realArgs)
    }
  }

  private def parseGoalAnnotation(variables: Map[CSPOMVariable[_], Variable]): PartialFunction[FZAnnotation, Heuristic] = {
    case a if a.predAnnId == "seq_search" =>
      val Seq(strategies: FZArrayExpr[_]) = a.expr
      new SeqHeuristic(
        strategies.value.map {
          case a: FZAnnotation => parseGoalAnnotation(variables)(a)
          case a => sys.error(s"Annotation expected in $strategies, found $a")
        }
          .toList)

    case a if a.predAnnId == "int_search" || a.predAnnId == "bool_search" =>
      val Seq(p, vca, aa, strategyannotation) = a.expr
      val CSPOMSeq(pool) = ann2expr(cspom, p)
      val decisionVariables = pool.collect {
        case v: CSPOMVariable[_] => variables(v)
      }
        .toArray

      val FZAnnotation(varchoiceannotation, _) = vca

      val varh: VariableHeuristic = varchoiceannotation match {
        case "input_order" => new LexVar(pm, decisionVariables)
        case "first_fail" => new Dom(pm, decisionVariables)
        case "antifirst_fail" => new MaxDom(pm, decisionVariables)
        case "smallest" => new SmallestValue(pm, decisionVariables)
        case "largest" => new LargestValue(pm, decisionVariables)
        case "occurrence" => new DDeg(pm, decisionVariables)
        case "most_constrained" => new Brelaz(pm, decisionVariables)
        case "max_regret" => new MaxRegret(pm, decisionVariables)
        case "free" => CrossHeuristic.defaultVar(pm, decisionVariables)
        case h =>
          logger.warn(s"Unsupported varchoice $h")
          CrossHeuristic.defaultVar(pm, decisionVariables)

      }

      val FZAnnotation(assignmentannotation, _) = aa

      val valh = assignmentannotation match {
        case "indomain" => new Lexico(pm)
        case "indomain_min" => new Lexico(pm)
        case "indomain_max" => new RevLexico(pm)
        case "indomain_median" => new MedValue(pm)
        case "indomain_random" => new RandomValue(pm)
        case "indomain_split" => new Split(pm)
        case "indomain_reverse_split" => new RevSplit(pm)
        case "indomain_interval" => new IntervalBranch(pm)
        case "indomain_free" => CrossHeuristic.defaultVal(pm)
        case h =>
          logger.warn(s"Unsupported assignment heuristic $h")
          CrossHeuristic.defaultVal(pm)
      }

      CrossHeuristic(varh, valh)
  }

  def parseGoal(goal: WithParam[CSPOMGoal[_]], variables: Map[CSPOMVariable[_], Variable]): Seq[Heuristic] = {
    goal.getSeqParam[FZAnnotation]("fzSolve")
      .collect(parseGoalAnnotation(variables))
  }

  override def loadCSPOM(args: List[String], opt: Map[Symbol, Any]) = {
    val List(fn) = args
    file = CSPOM.file2url(fn.replace(" ", "%20"))

    val (tryLoad, time) = StatisticsManager.measureTry(CSPOM.load(file, FlatZincParser))

    parseTime = time

    tryLoad.map { cspom =>

      outputVars = cspom.expressionsWithNames.collect {
        case (n, e) if cspom.getAnnotations(n).getSeqParam[FZAnnotation]("fzAnnotations").exists {
          case FZAnnotation("output_var", Seq()) => true
          case _ => false
        } => n
      }.toSeq

      outputArrays = cspom.expressionsWithNames.toMap.flatMap {
        case (n, e) =>
          cspom.getAnnotations(n).getSeqParam[FZAnnotation]("fzAnnotations").collectFirst {
            case FZAnnotation("output_array", Seq(data: FZArrayExpr[_])) =>
              val FZArrayExpr(array) = data
              val ranges = array.map {
                case FZSetConst(range) => range
                case _ => throw new InvalidParameterException("An array of set constants is expected here: " + data)
              }
              n -> ranges
          }
      }

      cspom
    }
      .flatMap { cspom =>
        CSPOMCompiler.compile(cspom, FZPatterns())
      }
  }

  override def applyParametersPre(problem: Problem, opt: Map[Symbol, Any]): Unit = {
    if (opt.contains('free)) {
      pm("free") = Unit
    } else if (opt.contains('totalFree)) {
      pm("totalFree") = Unit
    }

    val heuristics = if (pm.contains("totalFree")) {
      Seq()
    } else {
      val parsed = parseGoal(cspom.goal.get, variables)
      if (pm.contains("free")) {
        val decision = parsed.flatMap(_.decisionVariables).distinct
        Seq(Heuristic.default(pm, decision.toArray))
      } else {
        parsed
      }
    }

    val decisionVariables: Set[Variable] = heuristics
      .flatMap(_.decisionVariables)
      .toSet
    //.flatten

    val completed = if (decisionVariables.size < variables.size) {
      val remainingVariables = problem.variables.filterNot(decisionVariables)

      if (heuristics.isEmpty || heuristics.exists(_.shouldRestart)) {
        heuristics :+ CrossHeuristic(pm, remainingVariables)
      } else {
        /* Avoid introducing restarts if all defined heuristics do not enforce it */
        heuristics :+ CrossHeuristic(pm, remainingVariables).copy(shouldRestart = false)
      }
    } else {
      heuristics
    }

    val heuristic = completed match {
      case Seq(h) => h
      case m => new SeqHeuristic(m.toList)
    }

    logger.info(heuristic.toString + ", should restart: " + heuristic.shouldRestart)

    pm("heuristic") = heuristic

  }

  override def applyParametersCSPOM(solver: CSPOMSolver, opt: Map[Symbol, Any]): Unit = {
    solver.applyGoal().get
  }

  private def ann2expr(cspom: CSPOM, e: FZExpr[_]): CSPOMExpression[_] = e match {
    case FZAnnotation(vars, Seq()) => cspom.expression(vars).get

    case FZArrayExpr(list) => list.map(ann2expr(cspom, _))

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
      case _ => throw new IllegalArgumentException(args.toString)
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

  private def bool2int(n: String, sol: Map[String, Any]): String = {
    val s = sol(n)
    cspom.expression(n).map {
      case IntExpression(e) if s == true => "1"
      case IntExpression(e) if s == false => "0"
      case _ => s.toString
    }
      .get
  }

  override def outputCSPOM(sol: Map[String, Any]) = {
    val out: Iterable[String] = outputVars.map {
      n => s"$n = ${bool2int(n, sol)} ;"
    } ++ outputArrays.map {
      case (n, ranges) =>
        val seq =
          cspom.expression(n).getOrElse(throw new MatchError(s"could not find $n in $cspom"))

        val initRange = seq.asInstanceOf[CSPOMSeq[_]].definedIndices

        require(initRange.size == flattenedSize(ranges))
        val solutions = initRange.map(i => bool2int(s"$n[$i]", sol))

        s"$n = array${ranges.size}d(${
          ranges.map { range =>
            range.headOption
              .flatMap(h => range.lastOption.map(l => s"$h..$l"))
              .getOrElse("{}")
          }
            .mkString("", ", ", ", ")
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
    //    run(args)
  }

}

