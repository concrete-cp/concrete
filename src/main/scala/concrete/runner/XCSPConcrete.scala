package concrete.runner

import java.net.URL

import concrete.generator.cspompatterns.XCSPPatterns
import concrete.{ParameterManager, Problem, Solver}
import cspom.compiler.CSPOMCompiler
import cspom.xcsp.XCSPParser
import cspom.{CSPOM, CSPOMGoal, ExpressionMap, WithParam}

import scala.util.{Failure, Success, Try}

object XCSPConcrete extends CSPOMRunner with App {

  var file: URL = _

  var declaredVariables: Seq[String] = _

  def loadCSPOM(pm: ParameterManager, args: Seq[String]): Try[CSPOM] = {
    val List(fn) = args
    file = CSPOM.file2url(fn)
    CSPOM.load(file, XCSPParser)
      .flatMap { cspom =>
        CSPOMCompiler.compile(cspom, XCSPPatterns())
      }
      .flatMap { cspom =>
        declaredVariables = cspom.goal.get match {
          case s@WithParam(_: CSPOMGoal[_], _) =>
            s.getSeqParam("variables")
          case _ =>
            return Failure(new IllegalArgumentException("Variable sequence not available"))
        }
        Success(cspom)
      }
  }


  def description(args: Seq[String]) =
    args match {
      case Seq(fileName) => fileName
      case _ => throw new IllegalArgumentException(args.toString)
    }

  override def outputCSPOM(cspom: ExpressionMap, solution: Map[String, Any], obj: Option[Any]): String = {
    declaredVariables.map(v => s"$v = ${solution(v)}").mkString("\n")
  }


  def buildSolver(pm: ParameterManager, problem: Problem, goal: WithParam[CSPOMGoal[_]], expressions: ExpressionMap): Solver = {
    Solver(problem, pm).get
  }


  run(args) match {
    case Unfinished(Some(e)) => throw e
    case _ =>
  }

  def controlCSPOM(solution: Map[String, Any], obj: Option[Any]) = ???
}

