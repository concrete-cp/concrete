package concrete.runner

import java.net.URL
import java.security.InvalidParameterException

import com.typesafe.scalalogging.LazyLogging
import concrete._
import concrete.generator.FailedGenerationException
import concrete.generator.cspompatterns.FZPatterns
import concrete.heuristic._
import concrete.heuristic.branch.{BranchHeuristic, IntervalBranch, RevSplit, Split}
import concrete.heuristic.value._
import concrete.heuristic.variable._
import cspom.CSPOM.seq2CSPOMSeq
import cspom._
import cspom.compiler.CSPOMCompiler
import cspom.flatzinc._
import cspom.variable.{CSPOMExpression, CSPOMSeq, CSPOMVariable, IntExpression}
import org.scalameter.Quantity

import scala.util.{Failure, Random, Try}

object FZConcrete extends CSPOMRunner with LazyLogging {


  var outputVars: Seq[String] = _

  var outputArrays: Map[String, Seq[Seq[Int]]] = _

  var file: URL = _
  @Statistic
  var parseTime: Quantity[Double] = _

  def loadCSPOM(pm: ParameterManager, args: Seq[String]): Try[CSPOM] = {
    Try {
      val Seq(fn) = args
      fn
    }
      .recoverWith {
        case _: MatchError =>
          Failure(new InvalidParameterException("Please give exactly one file as argument when running FZConcrete"))
      }
      .flatMap(file => loadCSPOMURL(pm, CSPOM.file2url(file)))
  }

  def loadCSPOMURL(pm: ParameterManager, file: URL): Try[CSPOM] = {
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

  def updateParams(pm: ParameterManager, cspom: CSPOM): ParameterManager = {
    val goal = cspom.goal.getOrElse(throw new FailedGenerationException("Goal is not defined"))
    val expressions = cspom.expressionMap
    pm.updated("heuristic", readHeuristic(pm, goal, expressions, variables))
  }

  def description(args: Seq[String]): String =
    args match {
      case Seq(fileName) => fileName
      case _ => throw new IllegalArgumentException(args.toString)
    }

  override def outputCSPOM(cspom: ExpressionMap, sol: CSPOMSolution, obj: Option[Variable]): String = {
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

  private def parseGoal(pm: ParameterManager, goal: Seq[FZAnnotation], expressions: ExpressionMap, variables: Map[CSPOMVariable[_], Variable], rand: Random): Seq[Heuristic] = {
    goal.collect(parseGoalAnnotation(pm, expressions, variables, rand))
  }

  private def useDefault(pm0: ParameterManager, h: Heuristic, rand: Random): Heuristic = {
    h match {
      case hs: SeqHeuristic => SeqHeuristic(hs.heuristics.map(useDefault(pm0, _, rand)))
      case _ =>
        val pm1 = pm0.updated("heuristic.variable.tieBreaker",
          SeqVariableHeuristic(SeqHeuristic.extractVariableHeuristics(Seq(h))))
        Heuristic.default(pm1, h.decisionVariables, rand).get
    }
  }

  private def readHeuristic(pm0: ParameterManager, goal: WithParam[CSPOMGoal[_]], cspom: ExpressionMap, variables: Map[CSPOMVariable[_], Variable]): Heuristic = {

    val rand = {
      val seed = pm0.getOrElse("randomseed", 0L) + pm0.getOrElse("iteration", 0)
      new scala.util.Random(seed)
    }

    val heuristic = if (pm0.contains("ff")) {
      Heuristic.default(pm0, variables.values.toSeq, rand).get
    } else {
      val parsed = SeqHeuristic(parseGoal(pm0, goal.getSeqParam[FZAnnotation]("fzSolve"), cspom, variables, rand))
      if (pm0.contains("f")) {
        if (pm0.contains("fKeepSeq")) {
          useDefault(pm0, parsed, rand)
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
          Heuristic.default(pm1, decision, rand).get
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
                                  variables: Map[CSPOMVariable[_], Variable],
                                  rand: Random
                                 ): PartialFunction[FZAnnotation, Heuristic] = {
    case a if a.predAnnId == "seq_search" =>
      val Seq(strategies: FZArrayExpr[_]) = a.expr
      new SeqHeuristic(
        strategies.value.map {
          case a: FZAnnotation => parseGoalAnnotation(pm, cspom, variables, rand)(a)
          case a: Any => sys.error(s"Annotation expected in $strategies, found $a")
        }
          .toList)

    case a if a.predAnnId == "int_search" || a.predAnnId == "bool_search" =>
      val Seq(p, vca, aa, _) = a.expr
      val CSPOMSeq(pool) = ann2expr(cspom, p)
      val decisionVariables = pool.collect {
        case v: CSPOMVariable[_] => variables(v)
      }

      val FZAnnotation(varchoiceannotation, _) = vca

      val varh: Seq[VariableHeuristic] = varchoiceannotation match {
        case "input_order" => Seq(new LexVar(decisionVariables))
        case "first_fail" => Seq(new Dom(decisionVariables))
        case "antifirst_fail" => Seq(new MaxDom(decisionVariables))
        case "smallest" => Seq(new SmallestValue(decisionVariables))
        case "largest" => Seq(new LargestValue(decisionVariables))
        case "occurrence" => Seq(new DDeg(decisionVariables))
        case "most_constrained" => Seq(new DDeg(decisionVariables), new Dom(decisionVariables))
        case "max_regret" => Seq(new MaxRegret(decisionVariables))
        case "free" => VariableHeuristic.default(pm, decisionVariables, rand).get
        case h =>
          logger.warn(s"Unsupported varchoice $h")
          VariableHeuristic.default(pm, decisionVariables, rand).get
      }

      val FZAnnotation(assignmentannotation, _) = aa

      val valh: BranchHeuristic = assignmentannotation match {
        case "indomain" => new ValueHeuristic()
        case "indomain_min" => new ValueHeuristic()
        case "indomain_max" => new ValueHeuristic(new RevLexico())
        case "indomain_median" => new ValueHeuristic(new MedValue())
        case "indomain_random" => new ValueHeuristic(new RandomValue(rand))
        case "indomain_split" => new Split()
        case "indomain_reverse_split" => new RevSplit()
        case "indomain_interval" => new IntervalBranch()
        case "indomain_free" => BranchHeuristic.default(pm, rand).get
        case h =>
          logger.warn(s"Unsupported assignment heuristic $h")
          BranchHeuristic.default(pm, rand).get
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

