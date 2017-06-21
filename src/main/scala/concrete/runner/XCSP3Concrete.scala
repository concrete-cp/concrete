package concrete
package runner

import java.net.URL

import concrete.generator.cspompatterns.XCSPPatterns
import concrete.heuristic.{CrossHeuristic, Heuristic, SeqHeuristic}
import cspom.compiler.CSPOMCompiler
import cspom.variable.CSPOMVariable
import cspom.xcsp.XCSP3Parser
import cspom.{CSPOM, CSPOMGoal, StatisticsManager, WithParam}

import scala.collection.mutable
import scala.util.{Failure, Success, Try}
import scala.xml.Elem

object XCSP3Concrete extends CSPOMRunner with App {
  lazy val cache = new mutable.HashMap[URL, XCSP3SolutionChecker]
  lazy val solutionChecker = new XCSP3SolutionChecker(file)
  var file: URL = _
  var declaredVariables: Seq[String] = _

  override def defaultWriter(opt: Map[Symbol, Any], sm: StatisticsManager): ConcreteWriter = {
    new XCSP3Writer(opt, sm)
  }

  def loadCSPOM(args: List[String], opt: Map[Symbol, Any]): Try[CSPOM] = {
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

  def loadCSPOMURL(file: URL): Try[CSPOM] = {
    CSPOM.load(file, XCSP3Parser)
      .flatMap { cspom =>
        CSPOMCompiler.compile(cspom, XCSPPatterns())
      }
  }

  override def applyParametersPre(p: Problem, opt: Map[Symbol, Any]): Unit = {
    val heuristics = if (pm.contains("totalFree")) {
      Seq()
    } else {
      val dv = declaredVariables
        .flatMap(cspom.expression(_))
        .collect {
          case v: CSPOMVariable[_] => variables(v)
        }

      Seq(Heuristic.default(pm, dv.toArray))
    }

    val decisionVariables: Set[Variable] = heuristics
      .flatMap(_.decisionVariables)
      .toSet

    val completed = if (decisionVariables.size < variables.size) {
      val remainingVariables = p.variables.filterNot(decisionVariables)

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

  def description(args: List[String]) =
    args match {
      case List(fileName) => fileName
      case _ => throw new IllegalArgumentException(args.toString)
    }

  def controlCSPOM(solution: Map[String, Any], obj: Option[Any]) = {
    solutionChecker.checkSolution(solution, obj, declaredVariables)
  }

  override def outputCSPOM(solution: Map[String, Any], obj: Option[Any]): String = {
    xmlSolution(declaredVariables, solution, obj).toString
  }


  def xmlSolution(variables: Seq[String], solution: Map[String, Any], obj: Option[Any]): Elem = {
    <instantiation cost={obj.map(o => xml.Text(util.Math.any2Int(o).toString))}>
      <list>
        {variables.mkString(" ")}
      </list>
      <values>
        {variables.map(v => util.Math.any2Int(solution(v))).mkString(" ")}
      </values>
    </instantiation>
  }

  run(args)

}
