package concrete.runner

import java.net.URL
import java.security.InvalidParameterException

import com.typesafe.scalalogging.LazyLogging
import concrete._
import concrete.generator.cspompatterns.FZPatterns
import concrete.heuristic._
import concrete.heuristic.value._
import concrete.heuristic.variable._
import cspom.CSPOM.seq2CSPOMSeq
import cspom._
import cspom.compiler.CSPOMCompiler
import cspom.flatzinc._
import cspom.variable.{CSPOMExpression, CSPOMSeq, CSPOMVariable, IntExpression}
import org.scalameter.Quantity

import scala.util.Try

object FZConcrete extends CSPOMRunner with LazyLogging {


  var outputVars: Seq[String] = _

  var outputArrays: Map[String, Seq[Seq[Int]]] = _

  var file: URL = _
  @Statistic
  var parseTime: Quantity[Double] = _

  override def loadCSPOM(pm: ParameterManager, args: Seq[String]): Try[CSPOM] = {
    val Seq(fn) = args
    file = CSPOM.file2url(fn.replace(" ", "%20"))

    val (tryLoad, time) = StatisticsManager.measureTry[CSPOM, Unit, Double](CSPOM.load(file, FlatZincFastParser))

    parseTime = time

    tryLoad.map { cspom =>

      val allNames = cspom.expressionMap.allNames

      outputVars = allNames.iterator.filter { n =>
        cspom.getAnnotations(n).getSeqParam[FZAnnotation]("fzAnnotations").exists {
          case FZAnnotation("output_var", Seq()) => true
          case _ => false
        }
      }
        .toSeq

      outputArrays = allNames.flatMap { n =>
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
        .toMap

      cspom
    }
      .flatMap { cspom =>
        CSPOMCompiler.compile(cspom, FZPatterns())
      }
  }

  def buildSolver(pm: ParameterManager, problem: Problem,
                  goal: WithParam[CSPOMGoal[_]], expressions: ExpressionMap): Solver = {
    val heuristic = readHeuristic(pm, goal, expressions)
    Solver(problem, pm.updated("heuristic", heuristic))
  }

  def description(args: Seq[String]): String =
    args match {
      case Seq(fileName) => fileName
      case _ => throw new IllegalArgumentException(args.toString)
    }

  override def outputCSPOM(cspom: ExpressionMap, sol: Map[String, Any], obj: Option[Any]): String = {
    val out: Iterable[String] = outputVars.map {
      n => s"$n = ${bool2int(cspom, n, sol)} ;"
    } ++ outputArrays.map {
      case (n, ranges) =>
        val seq =
          cspom.expression(n).getOrElse(throw new MatchError(s"could not find $n in $cspom"))

        val initRange = seq.asInstanceOf[CSPOMSeq[_]].definedIndices

        require(initRange.size == flattenedSize(ranges))
        val solutions = initRange.map(i => bool2int(cspom, s"$n[$i]", sol))

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

  def controlCSPOM(solution: Map[String, Any], obj: Option[Any]): Option[String] = {
    throw new UnsupportedOperationException("Control of FZN solutions is not implemented")
  }

  def main(args: Array[String]): Unit = {
    run(args) match {
      case Unfinished(e) => e.foreach(throw _)
      case _ =>
    }
  }

  private def parseGoal(pm: ParameterManager, goal: Seq[FZAnnotation], expressions: ExpressionMap): Seq[Heuristic] = {
    goal
      .collect(parseGoalAnnotation(pm, expressions, variables))
  }

  private def useDefault(pm0: ParameterManager, h: Heuristic): Heuristic = {
    h match {
      case hs: SeqHeuristic => SeqHeuristic(hs.heuristics.map(useDefault(pm0, _)))
      case _ =>
        val pm1 = pm0.updated("heuristic.variable.tieBreaker",
          SeqVariableHeuristic(SeqHeuristic.extractVariableHeuristics(Seq(h))))
        Heuristic.default(pm1, h.decisionVariables)
    }
  }

  private def readHeuristic(pm0: ParameterManager, goal: WithParam[CSPOMGoal[_]], cspom: ExpressionMap): Heuristic = {
    val heuristic = if (pm0.contains("ff")) {
      Heuristic.default(pm0, variables.values.toSeq)
    } else {
      val parsed = SeqHeuristic(parseGoal(pm0, goal.getSeqParam[FZAnnotation]("fzSolve"), cspom))
      if (pm0.contains("f")) {
        if (pm0.contains("fKeepSeq")) {
          useDefault(pm0, parsed)
        } else {
          val decision = parsed.decisionVariables.distinct

          val pm1 = {
            if (pm0.contains("heuristic.variable.tieBreaker")) {
              pm0
            } else {
              val variableHeuristics = SeqHeuristic.extractVariableHeuristics(Seq(parsed))
              pm0.updated("heuristic.variable.tieBreaker", SeqVariableHeuristic(variableHeuristics))
            }
          }
          Heuristic.default(pm1, decision)
        }
      } else {
        parsed
      }
    }

    //val heuristic = SeqHeuristic(heuristics)

    logger.info(heuristic.toString + ", should restart: " + heuristic.shouldRestart)

    heuristic

  }

  private def parseGoalAnnotation(pm: ParameterManager, cspom: ExpressionMap,
                                  variables: Map[CSPOMVariable[_], Variable]): PartialFunction[FZAnnotation, Heuristic] = {
    case a if a.predAnnId == "seq_search" =>
      val Seq(strategies: FZArrayExpr[_]) = a.expr
      new SeqHeuristic(
        strategies.value.map {
          case a: FZAnnotation => parseGoalAnnotation(pm, cspom, variables)(a)
          case a: Any => sys.error(s"Annotation expected in $strategies, found $a")
        }
          .toList)

    case a if a.predAnnId == "int_search" || a.predAnnId == "bool_search" =>
      val Seq(p, vca, aa, _) = a.expr
      val CSPOMSeq(pool) = ann2expr(cspom, p)
      val decisionVariables = pool.collect {
        case v: CSPOMVariable[_] => variables(v)
      }

      val tieBreaker = new LexVar(decisionVariables)

      val FZAnnotation(varchoiceannotation, _) = vca

      val varh: VariableHeuristic = varchoiceannotation match {
        case "input_order" => new LexVar(decisionVariables, false)
        case "first_fail" => new Dom(decisionVariables, tieBreaker, false)
        case "antifirst_fail" => new MaxDom(decisionVariables, tieBreaker, false)
        case "smallest" => new SmallestValue(decisionVariables, tieBreaker, false)
        case "largest" => new LargestValue(decisionVariables, tieBreaker, false)
        case "occurrence" => new DDeg(decisionVariables, tieBreaker, false)
        case "most_constrained" => new DDeg(decisionVariables, new Dom(Seq(), tieBreaker), false)
        case "max_regret" => new MaxRegret(decisionVariables, tieBreaker, false)
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

  private def ann2expr(cspom: ExpressionMap, e: FZExpr[_]): CSPOMExpression[_] = e match {
    case FZAnnotation(vars, Seq()) => cspom.expression(vars).get

    case FZArrayExpr(list) => list.map(ann2expr(cspom, _))

    case FZArrayIdx(array, idx) => cspom.expression(array)
      .collect {
        case s: CSPOMSeq[_] => s(idx)
      }
      .get

    case e: Any => throw new InvalidParameterException("Cannot read search variable list in " + e)
  }

  //  private def flattenArrayExpr(ranges: Seq[Seq[Int]], name: String, solution: Map[String, Int]): Seq[Int] = {
  //    if (ranges.isEmpty) {
  //      Seq(solution(name))
  //    } else {
  //      ranges.head.flatMap(
  //        i => flattenArrayExpr(ranges.tail, s"$name[$i]", solution))
  //    }
  //  }

  private def flattenedSize(ranges: Seq[Seq[Int]]): Int =
    if (ranges.isEmpty) {
      1
    } else {
      ranges.head.size * flattenedSize(ranges.tail)
    }

  private def bool2int(cspom: ExpressionMap, n: String, sol: Map[String, Any]): String = {
    val s = sol(n)
    cspom.expression(n).map {
      case IntExpression(_) if s == true => "1"
      case IntExpression(_) if s == false => "0"
      case _ => s.toString
    }
      .get
  }

}

