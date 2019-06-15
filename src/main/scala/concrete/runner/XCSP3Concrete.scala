package concrete
package runner

import java.net.URL
import java.security.InvalidParameterException

import concrete.generator.cspompatterns.XCSPPatterns
import concrete.heuristic.Heuristic
import cspom._
import cspom.compiler.CSPOMCompiler
import cspom.variable.{CSPOMExpression, CSPOMVariable}
import cspom.xcsp.XCSP3Parser

import scala.collection.mutable
import scala.util.{Failure, Random, Success, Try}

object XCSP3Concrete extends CSPOMRunner with App {
  lazy val cache = new mutable.HashMap[URL, XCSP3SolutionChecker]
  lazy val solutionChecker = new XCSP3SolutionChecker(file)
  var file: URL = _
  var declaredVariables: Seq[String] = _

  override def defaultWriter(pm: ParameterManager, problem: String, sm: StatisticsManager): ConcreteWriter = {
    new XCSP3Writer(pm, problem, sm)
  }

  def loadCSPOM(pm: ParameterManager, args: Seq[String]): Try[CSPOM] = {
    Try {
      val Seq(fn) = args
      fn
    }
      .recoverWith {
        case _: MatchError =>
          Failure(new InvalidParameterException("Please give exactly one file as argument when running XCSP3Concrete"))
      }
      .flatMap(file => loadCSPOMURL(CSPOM.file2url(file)))
  }

  def loadCSPOMURL(file: URL): Try[CSPOM] = {
    this.file = file
    CSPOM.load(file, XCSP3Parser)
      .flatMap { cspom =>
        CSPOMCompiler.compile(cspom, XCSPPatterns())
      }
      .flatMap { cspom =>
        cspom.goal.get match {
          case s@WithParam(_: CSPOMGoal[_], _) =>
            declaredVariables = s.getParam[Seq[(String, CSPOMExpression[_])]]("variables")
              .get.map(_._1)

            Success(cspom)
          case _ =>
            Failure(new IllegalArgumentException("Variable sequence not available"))
        }
      }
  }

  def updateParams(pm: ParameterManager, cspom: CSPOM): ParameterManager = {
    pm.updated("heuristic", readHeuristic(pm, cspom))
  }

  private def readHeuristic(pm: ParameterManager, cspom: CSPOM): Heuristic = {
    val rand = {
      val seed = pm.getOrElse("randomseed", 0L) + pm.getOrElse("iteration", 0)
      new Random(seed)
    }


    val heuristic = if (pm.contains("ff")) {
      Heuristic.default(pm, variables.values.toSeq, rand)
    } else {
      val goalVar = cspom.goal.flatMap(g => g.obj.expr)

      val annotationDecision: Option[Seq[String]] = cspom.goal.flatMap(_.params.get("annotationDecision"))
          .map {
            case ann: Seq[String] @unchecked => ann
          }

      // If decision variables not annoted, use declared (non-auxiliary) variables
      val dv = annotationDecision.getOrElse(declaredVariables).flatMap(cspom.expression)
        .collect {
          // Do not assign goal variable
          case v: CSPOMVariable[_] if !goalVar.contains(v) => variables(v)
        }

      Heuristic.default(pm, dv, rand)
    }

    // logger.warn(heuristic.toString)

    heuristic.get
  }

  def description(args: Seq[String]): String =
    args match {
      case Seq(fileName) => fileName
      case _ => throw new IllegalArgumentException(args.toString)
    }

  def controlCSPOM(solution: Map[String, Any], obj: Option[Any]): Option[String] = {
    solutionChecker.checkSolution(solution, obj, declaredVariables)
  }

  override def outputCSPOM(cspom: ExpressionMap, solution: CSPOMSolution, obj: Option[Variable]): String = {
    xmlSolution(declaredVariables, solution, obj.flatMap(solution.get))
  }


  def xmlSolution(variables: Seq[String], solution: Map[String, Any], obj: Option[Any]): String = {
    s"""<instantiation
        ${obj.map(o => s"cost = '${concrete.util.Math.any2Int(o)}'").getOrElse("")}>
      <list>
        ${variables.mkString(" ")}
      </list>
      <values>
        ${variables.map(v => concrete.util.Math.any2Int(solution(v))).mkString(" ")}
      </values>
    </instantiation>"""
  }

  run(args) match {
    case Unfinished(Some(e)) => throw e
    case _ =>
  }

}
