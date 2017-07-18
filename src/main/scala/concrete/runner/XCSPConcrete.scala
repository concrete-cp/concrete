package concrete.runner

import java.net.URL

import concrete.CSPOMSolver
import concrete.generator.cspompatterns.XCSPPatterns
import cspom.{CSPOM, CSPOMGoal, WithParam}
import cspom.compiler.CSPOMCompiler
import cspom.xcsp.XCSPParser

import scala.util.{Failure, Success, Try}

object XCSPConcrete extends CSPOMRunner with App {

  var file: URL = _

  var declaredVariables: Seq[String] = _

  def loadCSPOM(args: List[String], opt: Map[Symbol, Any]): Try[CSPOM] = {
    val List(fn) = args
    file = CSPOM.file2url(fn)
    loadCSPOMURL(file).flatMap { cspom =>
      declaredVariables = cspom.goal.get match {
        case s@WithParam(_: CSPOMGoal[_], _) =>
          s.getSeqParam("variables")
        case _ =>
          return Failure(new IllegalArgumentException("Variable sequence not available"))
      }
      Success(cspom)
    }
  }

  def loadCSPOMURL(file: URL): Try[CSPOM] = {
    CSPOM.load(file, XCSPParser)
      .flatMap { cspom =>
        CSPOMCompiler.compile(cspom, XCSPPatterns())
      }
  }

  override def applyParametersCSPOM(solver: CSPOMSolver, opt: Map[Symbol, Any]): Unit = {
    solver.applyGoal().get
  }

  def description(args: List[String]) =
    args match {
      case List(fileName) => fileName
      case _ => throw new IllegalArgumentException(args.toString)
    }

  override def outputCSPOM(solution: Map[String, Any], obj: Option[Any]): String = {
    declaredVariables.map(v => s"$v = ${solution(v)}").mkString("\n")
  }

  run(args)

  def controlCSPOM(solution: Map[String, Any], obj: Option[Any]) = ???
}

