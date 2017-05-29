package concrete
package runner

import java.net.URL

import concrete.generator.cspompatterns.XCSPPatterns
import cspom.compiler.CSPOMCompiler
import cspom.xcsp.XCSP3Parser
import cspom.{CSPOM, CSPOMGoal, StatisticsManager, WithParam}

import scala.util.{Failure, Success, Try}
import scala.xml.Elem

object XCSP3Concrete extends CSPOMRunner with App {
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

  override def applyParametersCSPOM(solver: CSPOMSolver, opt: Map[Symbol, Any]): Unit = {
    solver.applyGoal().get
  }

  def description(args: List[String]) =
    args match {
      case List(fileName) => fileName
      case _ => throw new IllegalArgumentException(args.toString)
    }

  def controlCSPOM(solution: Map[String, Any], obj: Option[Any]) = {
    controlCSPOM(solution, obj, declaredVariables, file)
  }

  def controlCSPOM(sol: Map[String, Any], obj: Option[Any], variables: Seq[String], url: URL) = {
    new XCSP3SolutionChecker(url).checkSolution(xmlSolution(variables, sol, obj))
  }

  override def outputCSPOM(solution: Map[String, Any], obj: Option[Any]): String = {
    xmlSolution(declaredVariables, solution, obj).toString
  }

  def xmlSolution(variables: Seq[String], solution: Map[String, Any], obj: Option[Any]): Elem = {
    val i = <instantiation cost={obj.map(o => xml.Text(util.Math.any2Int(o).toString))}>
      <list>
        {variables.mkString(" ")}
      </list>
      <values>
        {variables.map(v => util.Math.any2Int(solution(v))).mkString(" ")}
      </values>
    </instantiation>

    println(i)

    i
  }

  run(args)

}
