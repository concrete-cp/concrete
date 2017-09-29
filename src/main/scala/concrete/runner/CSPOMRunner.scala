package concrete.runner

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import concrete._
import concrete.generator.cspompatterns.ConcretePatterns
import concrete.generator.{FailedGenerationException, ProblemGenerator}
import cspom._
import cspom.compiler.CSPOMCompiler
import cspom.variable.CSPOMVariable

import scala.util.Try

trait CSPOMRunner extends ConcreteRunner {
  var variables: Map[CSPOMVariable[_], Variable] = _

  var cspomSolver: CSPOMSolver = _

  override final def output(sol: Map[Variable, Any], obj: Option[Any]) =
    outputCSPOM(cspomSolver.cspom, cspomSolver.solution(sol), obj)

  def outputCSPOM(cspom: ExpressionMap, solution: Map[String, Any], obj: Option[Any]): String = {
    solution.iterator.map {
      case (variable, value) => s"${variable} = $value"
    }.mkString("\n")
  }

  override final def control(sol: Map[Variable, Any], obj: Option[Any]) =
    controlCSPOM(cspomSolver.solution(sol), obj)

  final def load(pm0: ParameterManager, args: Seq[String]): Try[Solver] = {
    loadCSPOM(pm0, args).flatMap(cspom => load(pm0, cspom))
  }

  def load(pm: ParameterManager, cspom: CSPOM): Try[Solver] = {
    for {
      cspom2 <- CSPOMCompiler.compile(cspom, ConcretePatterns(pm))
      (problem, vars) <- generate(pm, cspom2)
    } yield {
      //println(problem)
      variables = vars
      // logger.info(problem.initState.map(problem.toString).getOrElse("Contradiction").lines.map("% " + _).mkString("\n"))
      val solver = buildSolver(pm, problem,
        cspom2.goal.getOrElse(throw new FailedGenerationException("Goal is not defined")), cspom2.expressionMap)
      cspomSolver = new CSPOMSolver(solver, cspom2.expressionMap, vars)
      solver
    }
  }

  private def generate(pm: ParameterManager, cspom: CSPOM) = {
    for (f <- pm.get[String]("gml")) {
      Files.write(
        Paths.get(f),
        GML(cspom).getBytes(StandardCharsets.UTF_8))
    }

    val pg = new ProblemGenerator(pm)
    statistics.register("problemGenerator", pg)
    pg.generate(cspom)
  }

  def loadCSPOM(pm: ParameterManager, args: Seq[String]): Try[CSPOM]

  def buildSolver(pm: ParameterManager, problem: Problem, goal: WithParam[CSPOMGoal[_]], expressions: ExpressionMap): Solver

  def controlCSPOM(solution: Map[String, Any], obj: Option[Any]): Option[String]

}