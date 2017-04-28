package concrete.runner

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Paths

import scala.util.Try

import concrete.CSPOMSolver
import concrete.Problem
import concrete.Solver
import concrete.Variable
import concrete.generator.ProblemGenerator
import concrete.generator.cspompatterns.Bool2IntIsEq
import concrete.generator.cspompatterns.ConcretePatterns
import cspom.CSPOM
import cspom.GML
import cspom.compiler.CSPOMCompiler
import cspom.variable.CSPOMVariable

trait CSPOMRunner extends ConcreteRunner {

  var cspom: CSPOM = _

  var variables: Map[CSPOMVariable[_], Variable] = _

  var cspomSolver: CSPOMSolver = _

  override def options(args: List[String], o: Map[Symbol, Any] = Map.empty, realArgs: List[String]): (Map[Symbol, Any], List[String]) =
    args match {
      case "-gml" :: t :: tail => options(tail, o + ('gml -> t), realArgs)
      case _                   => super.options(args, o, realArgs)
    }

  final def load(args: List[String], opt: Map[Symbol, Any]): Try[Problem] = {
    loadCSPOM(args, opt)
      .flatMap { cspom =>
        this.cspom = cspom
        // println(cspom)
        CSPOMCompiler.compile(cspom, ConcretePatterns(pm))
      }
      .flatMap { cspom =>
        // println("BOOL2INTISEQ")
        CSPOMCompiler.compile(cspom, Seq(Bool2IntIsEq))
      }
      .flatMap { cspom =>

        for (f <- opt.get('gml)) {
          Files.write(
            Paths.get(f.toString),
            GML(cspom).getBytes(StandardCharsets.UTF_8))
        }

        val pg = new ProblemGenerator(pm)
        statistics.register("problemGenerator", pg)
        pg.generate(cspom)
      }
      .map {
        case (problem, vars) =>
          //println(problem)
          variables = vars
          // logger.info(problem.initState.map(problem.toString).getOrElse("Contradiction").lines.map("% " + _).mkString("\n"))
          problem
      }
  }

  override final def applyParametersPost(s: Solver, opt: Map[Symbol, Any]) = {
    cspomSolver = new CSPOMSolver(s, cspom, variables)
    applyParametersCSPOM(cspomSolver, opt)
  }

  def applyParametersCSPOM(s: CSPOMSolver, opt: Map[Symbol, Any]): Unit = {}

  def loadCSPOM(args: List[String], opt: Map[Symbol, Any]): Try[CSPOM]

  def outputCSPOM(solution: Map[String, Any]): String = {
    solution.iterator.map {
      case (variable, value) => s"${variable} = $value"
    }.mkString("\n")
  }

  override final def output(sol: Map[Variable, Any]) =
    outputCSPOM(cspomSolver.solution(sol))

  def controlCSPOM(solution: Map[String, Any]): Option[String]

  override final def control(sol: Map[Variable, Any]) =
    controlCSPOM(cspomSolver.solution(sol))

}