package concrete.runner

import concrete.CSPOMSolver
import concrete.Problem
import concrete.Solver
import concrete.Variable
import concrete.generator.ProblemGenerator
import concrete.generator.cspompatterns.Bool2IntIsEq
import concrete.generator.cspompatterns.ConcretePatterns
import cspom.CSPOM
import cspom.compiler.MergeEq
import cspom.compiler.CSPOMCompiler
import cspom.variable.CSPOMVariable
import scala.util.Try

trait CSPOMRunner extends ConcreteRunner {

  var cspom: CSPOM = _

  var variables: Map[CSPOMVariable[_], Variable] = _

  var cspomSolver: CSPOMSolver = _

  final def load(args: List[String], opt: Map[Symbol, Any]): Try[Problem] = {
    loadCSPOM(args, opt)
      .flatMap { cspom =>
        this.cspom = cspom
        // println(cspom)
        CSPOMCompiler.compile(cspom, ConcretePatterns(pm))

        CSPOMCompiler.compile(cspom, Seq(Bool2IntIsEq))

        //println(cspom)

        val pg = new ProblemGenerator(pm)
        statistics.register("problemGenerator", pg)
        pg.generate(cspom)
      }
      .map {
        case (problem, vars) =>
          //println(problem)
          variables = vars
          logger.info(problem.initState.toString(problem).lines.map("% " + _).mkString("\n"))
          problem
      }
  }

  override final def applyParameters(s: Solver, opt: Map[Symbol, Any]) = {
    cspomSolver = new CSPOMSolver(s, cspom, variables)
    applyParametersCSPOM(cspomSolver, opt)
  }

  def applyParametersCSPOM(s: CSPOMSolver, opt: Map[Symbol, Any]): Unit = {}

  def loadCSPOM(args: List[String], opt: Map[Symbol, Any]): Try[CSPOM]

  def outputCSPOM(solution: Map[String, Any]): String = {
    solution.map {
      case (variable, value) => s"${variable} = $value"
    }.mkString("\n")
  }

  override final def output(sol: Map[Variable, Any]) =
    outputCSPOM(cspomSolver.solution(sol))

  def controlCSPOM(solution: Map[String, Any]): Option[String]

  override final def control(sol: Map[Variable, Any]) =
    controlCSPOM(cspomSolver.solution(sol))

}