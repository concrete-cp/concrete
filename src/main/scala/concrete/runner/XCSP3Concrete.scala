package concrete
package runner

import java.net.URL

import concrete.generator.cspompatterns.XCSPPatterns
import concrete.heuristic.{CrossHeuristic, Heuristic, SeqHeuristic}
import cspom._
import cspom.compiler.CSPOMCompiler
import cspom.variable.CSPOMVariable
import cspom.xcsp.XCSP3Parser

import scala.collection.mutable
import scala.util.{Failure, Success, Try}
import scala.xml.Elem

object XCSP3Concrete extends CSPOMRunner with App {
  lazy val cache = new mutable.HashMap[URL, XCSP3SolutionChecker]
  lazy val solutionChecker = new XCSP3SolutionChecker(file)
  var file: URL = _
  var declaredVariables: Seq[String] = _

  override def defaultWriter(pm: ParameterManager, problem:String, sm: StatisticsManager): ConcreteWriter = {
    new XCSP3Writer(pm, problem, sm)
  }

  def loadCSPOM(pm: ParameterManager, args: Seq[String]): Try[CSPOM] = {
    val List(fn) = args
    file = CSPOM.file2url(fn)

    loadCSPOMURL(file).flatMap { cspom =>
      cspom.goal.get match {
        case s@WithParam(_: CSPOMGoal[_], _) =>
          declaredVariables = s.getSeqParam("variables")
          Success(cspom)
        case _ =>
          Failure(new IllegalArgumentException("Variable sequence not available"))
      }
    }

  }

  def buildSolver(pm: ParameterManager, problem: Problem, goal: WithParam[CSPOMGoal[_]], cspom: ExpressionMap) = {
    val heuristic = readHeuristic(pm, cspom)
    Solver(problem, pm.updated("heuristic", heuristic))
  }

  def loadCSPOMURL(file: URL): Try[CSPOM] = {
    CSPOM.load(file, XCSP3Parser)
      .flatMap { cspom =>
        CSPOMCompiler.compile(cspom, XCSPPatterns())
      }
  }

  private def readHeuristic(pm: ParameterManager, cspom: ExpressionMap): ParameterManager = {
    val heuristics = if (pm.contains("ff")) {
      Seq()
    } else {
      val dv = declaredVariables
        .flatMap(cspom.expression)
        .collect {
          case v: CSPOMVariable[_] => variables(v)
        }

      Seq(Heuristic.default(pm, dv))
    }

    val decisionVariables: Set[Variable] = heuristics
      .flatMap(_.decisionVariables)
      .toSet

    val completed = if (decisionVariables.size < variables.size) {
      val remainingVariables = variables.values.iterator.filterNot(decisionVariables).toArray

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

    pm.updated("heuristic", heuristic)

  }

  def description(args: Seq[String]) =
    args match {
      case Seq(fileName) => fileName
      case _ => throw new IllegalArgumentException(args.toString)
    }

  def controlCSPOM(solution: Map[String, Any], obj: Option[Any]) = {
    solutionChecker.checkSolution(solution, obj, declaredVariables)
  }

  override def outputCSPOM(cspom: ExpressionMap, solution: Map[String, Any], obj: Option[Any]): String = {
    xmlSolution(declaredVariables, solution, obj).toString
  }


  def xmlSolution(variables: Seq[String], solution: Map[String, Any], obj: Option[Any]): Elem = {
    <instantiation cost={obj.map(o => xml.Text(concrete.util.Math.any2Int(o).toString))}>
      <list>
        {variables.mkString(" ")}
      </list>
      <values>
        {variables.map(v => concrete.util.Math.any2Int(solution(v))).mkString(" ")}
      </values>
    </instantiation>
  }

  run(args) match {
    case Unfinished(Some(e)) => throw e
    case _ =>
  }

}
