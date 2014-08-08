package concrete.runner

import concrete.generator.cspompatterns.ConcretePatterns
import concrete.generator.ProblemGenerator
import cspom.variable.CSPOMVariable
import cspom.CSPOM
import concrete.Variable
import cspom.compiler.ProblemCompiler
import concrete.Problem
import concrete.CSPOMSolver
import concrete.Solver

trait CSPOMRunner extends ConcreteRunner {

  var cspom: CSPOM = _

  var variables: Map[CSPOMVariable[_], Variable] = _

  var cspomSolver: CSPOMSolver = _

  final def load(args: List[String]): Problem = {
    cspom = loadCSPOM(args)
    println(cspom)
    ProblemCompiler.compile(cspom, ConcretePatterns(pm))

    //println(cspom)
    val pg = new ProblemGenerator(pm)
    statistics.register("problemGenerator", pg)
    val (problem, vars) = pg.generate(cspom)
    variables = vars
    //println(problem)
    problem
  }

  override final def applyParameters(s: Solver) = {
    cspomSolver = new CSPOMSolver(s, cspom, variables)
    applyParametersCSPOM(cspomSolver)
  }

  def applyParametersCSPOM(s: CSPOMSolver): Unit = {}

  def loadCSPOM(args: List[String]): CSPOM

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