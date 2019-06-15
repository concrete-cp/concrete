package concrete.runner

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import concrete._
import concrete.generator.ProblemGenerator
import concrete.generator.cspompatterns.ConcretePatterns
import cspom._
import cspom.compiler.CSPOMCompiler
import cspom.variable.CSPOMVariable

import scala.util.Try

trait CSPOMRunner extends ConcreteRunner {
  var variables: Map[CSPOMVariable[_], Variable] = _

  var cspomSolver: CSPOMSolver = _

  override final def output(sol: Map[Variable, Any], obj: Option[Variable]): String =
    outputCSPOM(cspomSolver.cspom, cspomSolver.solution(sol), obj)

  def outputCSPOM(cspom: ExpressionMap, solution: CSPOMSolution, obj: Option[Variable]): String = {
    solution.iterator.map {
      case (variable, value) => s"$variable = $value"
    }.mkString("\n")
  }

  override final def control(sol: Map[Variable, Any], obj: Option[Any]): Option[String] =
    controlCSPOM(cspomSolver.solution(sol), obj)

  final def load(pm0: ParameterManager, args: Seq[String]): Try[Solver] = {
    loadCSPOM(pm0, args).flatMap(cspom => load(pm0, cspom))
  }

  def load(pm: ParameterManager, cspom: CSPOM): Try[Solver] = {
    for {
      cspom2 <- CSPOMCompiler.compile(cspom, ConcretePatterns(pm))
      problem <- generate(pm, cspom2)
      solver <- Solver(problem,
        problem.variables.filter(x => cspom.expressionMap.expression(x.name).isDefined).toSeq,
        updateParams(pm, cspom2))
    } yield {
      cspomSolver = new CSPOMSolver(solver, cspom2.expressionMap, variables)
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
    for ((problem, variables) <- pg.generate(cspom)) yield {
      this.variables = variables
      problem
    }
  }

  def loadCSPOM(pm: ParameterManager, args: Seq[String]): Try[CSPOM]

  def updateParams(pm: ParameterManager, cspom: CSPOM): ParameterManager

  def controlCSPOM(solution: Map[String, Any], obj: Option[Any]): Option[String]

}