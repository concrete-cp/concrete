/**
 * CSPFJ - CSP solving API for Java
 * Copyright (C) 2006 Julien VION
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 */

package concrete;

import java.util.logging.Logger
import scala.annotation.tailrec
import scala.collection.JavaConversions
import com.typesafe.scalalogging.slf4j.LazyLogging
import concrete.constraint.TupleEnumerator
import concrete.constraint.extension.ReduceableExt
import concrete.filter.Filter
import concrete.generator.ProblemGenerator
import concrete.generator.cspompatterns.ConcretePatterns
import cspom.CSPOM
import cspom.Statistic
import cspom.StatisticsManager
import cspom.compiler.ProblemCompiler
import cspom.variable.CSPOMVariable
import cspom.variable.CSPOMSeq
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMConstant

final class SolverFactory(val params: ParameterManager) {

  @Parameter("solver")
  var solverClass: Class[_ <: Solver] = classOf[MAC]

  params.register(this)

  def apply(problem: Problem): Solver = {
    solverClass.getConstructor(classOf[Problem]).newInstance(problem);
  }

  def apply(cspom: CSPOM): CSPOMSolver = {
    ProblemCompiler.compile(cspom, ConcretePatterns())
    val pg = new ProblemGenerator(params)
    val (problem, variables) = pg.generate(cspom)
    new CSPOMSolver(apply(problem), cspom, variables)
  }
}

object Solver {
  val VERSION = s"Concrete 2.0-SNAPSHOT"
  def apply(cspom: CSPOM, pm: ParameterManager = new ParameterManager): CSPOMSolver = new SolverFactory(pm)(cspom)
  def apply(problem: Problem): Solver = apply(problem, new ParameterManager)
  def apply(problem: Problem, pm: ParameterManager): Solver = new SolverFactory(pm)(problem)
}

class CSPOMSolver(
  private val solver: Solver,
  private val cspom: CSPOM,
  private val variables: Map[CSPOMVariable[_], Variable]) extends Iterator[Map[String, Any]] {

  def hasNext = solver.hasNext

  def next() = concrete2CspomSol(solver.next)

  def concrete2CspomSol(sol: Map[Variable, Any]): Map[String, Any] = {
    cspom.namedExpressions.flatMap {
      case (n, e) => concrete2CspomSol(n, e, sol)
    }
  }

  private def concrete2CspomSol(name: String, expr: CSPOMExpression[_], sol: Map[Variable, Any]): Map[String, Any] = {
    expr match {
      case seq: CSPOMSeq[_] => (seq.values zip seq.definedIndices).flatMap {
        case (v, i) => concrete2CspomSol(s"$name[$i]", v, sol)
      } toMap
      case const: CSPOMConstant[_] => Map(name -> const.value)
      case variable: CSPOMVariable[_] => Map(name -> sol(variables(variable)))
    }
  }

  def maximize(v: String) = solver.maximize(variables(cspom.variable(v)))

  def minimize(v: String) = solver.minimize(variables(cspom.variable(v)))

  def concreteProblem = solver.problem

  def statistics = solver.statistics
}

abstract class Solver(val problem: Problem, val params: ParameterManager) extends Iterator[Map[Variable, Any]] with LazyLogging {

  params.register(this)

  @Statistic
  var preproRemoved = 0
  @Statistic
  var preproCpu = 0.0

  @Statistic
  val nbConstraints = problem.constraints.size

  @Statistic
  val nbVariables = problem.variables.size

  @Parameter("preprocessor")
  var preprocessorClass: Class[_ <: Filter] = null

  val statistics = new StatisticsManager
  statistics.register("solver", this)
  //statistics.register("domains", IntDomain)
  statistics.register("enumerator", TupleEnumerator)
  statistics.register("relation", ReduceableExt)
  statistics.register("domain", Domain)

  private var _next: SolverResult = UNKNOWNResult

  private var _minimize: Option[Variable] = None
  private var _maximize: Option[Variable] = None
  def minimize(v: Variable) { _maximize = None; _minimize = Some(v) }
  def maximize(v: Variable) { _minimize = None; _maximize = Some(v) }

  def isOptimizer = _maximize.nonEmpty || _minimize.nonEmpty

  def next() = _next match {
    case UNSAT => Iterator.empty.next
    case UNKNOWNResult => if (hasNext) next() else Iterator.empty.next
    case SAT(sol) =>
      _next = UNKNOWNResult
      for (v <- _maximize) {
        reset()
        try {
          v.dom.removeTo(v.dom.index(sol(v).asInstanceOf[Int]))
        } catch {
          case e: UNSATException => _next = UNSAT
        }
      }
      for (v <- _minimize) {
        reset()
        try {
          v.dom.removeFrom(v.dom.index(sol(v).asInstanceOf[Int]))
        } catch {
          case e: UNSATException => _next = UNSAT
        }
      }
      sol
    case RESTART => throw new IllegalStateException()
  }

  protected def nextSolution(): SolverResult

  def hasNext = _next match {
    case UNSAT => false
    case SAT(_) => true
    case UNKNOWNResult =>
      _next = nextSolution(); hasNext
    case RESTART => throw new IllegalStateException
  }

  //  @tailrec
  //  private def bestSolution(v: Variable, best: SolverResult): SolverResult = nextSolution() match {
  //    case SAT(sol) =>
  //      logger.info("New bound " + sol(v.name))
  //      reset()
  //      v.dom.removeTo(v.dom.index(sol(v.name)))
  //      bestSolution(v, SAT(sol))
  //    case UNSAT => if (best == UNKNOWNResult) UNSAT else best
  //    case _ => best
  //  }
  //
  //  def bestSolution(v: Variable): SolverResult = bestSolution(v, UNKNOWNResult)

  private var _maxBacktracks = -1

  //var preproExpiration = -1

  def reset()

  protected def extractSolution: Map[Variable, Any] = problem.variables.map(v => (v, v.dom)).map {
    case (variable, dom: IntDomain) => variable -> dom.firstValue
    case (variable, dom: BooleanDomain) => variable -> dom.canBe(true)
  } toMap

  final def preprocess(filter: Filter): Boolean = {

    logger.info("Preprocessing");

    val preprocessor = if (preprocessorClass == null) {
      filter
    } else {
      val p = preprocessorClass.getConstructor(classOf[Problem]).newInstance(problem)
      statistics.register("preprocessor", p)
      p
    }

    val (r, t) = StatisticsManager.time(preprocessor.reduceAll());

    preproRemoved = problem.variables map { v => v.dom.maxSize - v.dom.size } sum

    this.preproCpu = t;
    r

  }

  def xmlConfig = params.toXML

}

sealed trait SolverResult {
  def isSat: Boolean
  def get: Map[Variable, Any]
  def getNum: Map[Variable, Number] = get map {
    case (s, i) => (s, i.asInstanceOf[Number])
  }
  def getInteger: java.util.Map[Variable, java.lang.Integer] = JavaConversions.mapAsJavaMap(get map {
    case (s, i) => (s, i.asInstanceOf[java.lang.Integer])
  })
}

case class SAT(val solution: Map[Variable, Any]) extends SolverResult {
  def isSat = true
  def get = solution
  override def toString = "SAT: " + solution.toString
}
case object UNSAT extends SolverResult {
  def isSat = false
  def get = throw new NoSuchElementException
  override def toString = "UNSAT"
}
case object UNKNOWNResult extends SolverResult {
  def isSat = false
  def get = throw new NoSuchElementException
  override def toString = "UNKNOWN"
}
case object RESTART extends SolverResult {
  def isSat = false
  def get = throw new NoSuchElementException
  override def toString = "RESTART"
}
