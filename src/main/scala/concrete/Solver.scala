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

package concrete

import com.typesafe.scalalogging.LazyLogging
import concrete.constraint.extension.{BinaryExt, ReduceableExt}
import concrete.constraint.linear.LinearLe
import concrete.constraint.semantic.DiffN
import concrete.constraint.{Constraint, TupleEnumerator}
import concrete.filter.Filter
import concrete.generator.ProblemGenerator
import concrete.generator.cspompatterns.ConcretePatterns
import cspom.compiler.CSPOMCompiler
import cspom.{CSPOM, Statistic, StatisticsManager}
import scala.jdk.CollectionConverters._

import scala.util.Try

object Solver {
  def apply(cspom: CSPOM): Try[CSPOMSolver] = apply(cspom, new ParameterManager)

  def apply(cspom: CSPOM, pm: ParameterManager): Try[CSPOMSolver] = {
    for {
      cspom <- CSPOMCompiler.compile(cspom, ConcretePatterns(pm))
      pg = new ProblemGenerator(pm)
      (problem, variables) <- pg.generate(cspom)
      solver <- apply(problem,
        problem.variables.toSeq.filter(x => cspom.expressionMap.expression(x.name).isDefined), pm)
    } yield {
      solver.statistics.register("compiler", CSPOMCompiler)
      solver.statistics.register("generator", pg)

      new CSPOMSolver(solver, cspom.expressionMap, variables)
    }
  }

  def apply(problem: Problem, decisionVariables: Seq[Variable], pm: ParameterManager): Try[Solver] = {
    val solverClass: Class[_ <: Solver] =
      pm.classInPackage("solver", "concrete", classOf[MAC])

    solverClass
      .getMethod("apply", classOf[Problem], classOf[Seq[_]], classOf[ParameterManager])
      .invoke(null, problem, decisionVariables, pm)
      .asInstanceOf[Try[Solver]]
  }

  def apply(problem: Problem): Try[Solver] = apply(problem, problem.variables.toSeq, new ParameterManager)
}

abstract class Solver(val problem: Problem, val params: ParameterManager)
  extends Iterator[Map[Variable, Any]] with LazyLogging {

  @Statistic
  val nbConstraints: Int = problem.constraints.length


  @Statistic
  val nbVariables: Int = problem.variables.length
  val preprocessorClass: Option[Class[_ <: Filter]] =
    params.get[Class[_ <: Filter]]("preprocessor")
  @Statistic
  val statistics = new StatisticsManager
  @Statistic
  var preproRemoved: Long = -1L

  //  def decisionVariables(dv: Seq[Variable]): Unit = {
  //    logger.info(s"Decision variables: $dv")
  //    problem.decisionVariables = dv.toList
  //    for (v <- optimises) {
  //      if (!problem.decisionVariables.contains(v)) problem.decisionVariables +:= v
  //    }
  //  }
  @Statistic
  var preproCpu: Double = _
  //statistics.register("solver", this)
  //statistics.register("domains", IntDomain)
  statistics.register("enumerator", TupleEnumerator)
  statistics.register("relation", ReduceableExt)
  statistics.register("domain", Domain)
  statistics.register("linear", LinearLe)
  statistics.register("binary", BinaryExt)
  statistics.register("diffn", DiffN)
  var running = false
  var optimConstraint: Option[Constraint] = None
  @Statistic
  var nbSolutions = 0
  private var _next: SolverResult = UNKNOWNResult(None)

  def optimises: Option[Variable] = problem.goal.optimizes

  def obtainOptimConstraint[A <: Constraint](f: => A): A = {
    optimConstraint
      .map {
        case c: A@unchecked => c
      }
      .getOrElse {
        val oc = f
        optimConstraint = Some(oc)
        addConstraint(oc)
      }
  }

  def addConstraint[A <: Constraint](c: A): A = {
    problem.addConstraint(c)
    c
  }

  def next(): Map[Variable, Any] = _next match {
    case UNSAT => Iterator.empty.next
    case UNKNOWNResult(None) => if (hasNext) next() else Iterator.empty.next
    case SAT(sol) =>
      nbSolutions += 1
      _next = UNKNOWNResult(None)
      sol
    case RESTART => throw new IllegalStateException()
    case UNKNOWNResult(Some(e)) => throw e
  }

  def nextJava(): java.util.Map[Variable, java.lang.Integer] = next()
    .map {
      case (v, i: Int) => (v, i: java.lang.Integer)
      case (v, i) => throw new NumberFormatException(s"$v = $i : should be an integer")
    }
    .asJava

  def javaIterator: java.util.Iterator[java.util.Map[Variable, java.lang.Integer]] = integerIterator.asJava

  def javaCollection: java.util.List[java.util.Map[Variable, java.lang.Integer]] = integerIterator.toSeq.asJava

  def integerIterator: Iterator[java.util.Map[Variable, java.lang.Integer]] = iterator.map {
    s =>
      s.map {
        case (v, i: Int) => (v, i: java.lang.Integer)
        case (v, i) => throw new NumberFormatException(s"$v = $i : should be an integer")
      }
        .asJava
  }

  def hasNext: Boolean = _next match {
    case UNSAT => false
    case SAT(_) => true
    case UNKNOWNResult(None) =>
      _next = nextSolution(); hasNext
    case UNKNOWNResult(Some(e)) => throw e
    case RESTART => throw new IllegalStateException
  }

  def reset(): Unit

  final def preprocess(filter: Filter, state: ProblemState): Outcome = {

    logger.info("Preprocessing");

    val preprocessor = preprocessorClass
      .map {
        pc =>
          val p = pc.getConstructor(classOf[Problem]).newInstance(problem)
          statistics.register("preprocessor", p)
          p
      }
      .getOrElse {
        filter
      }

    val (r, t) = StatisticsManager.measure(preprocessor.reduceAll(state))

    this.preproCpu = t
    //println(Thread.currentThread().getStackTrace.toSeq)

    r.get.andThen { newState =>
      preproRemoved = problem.variables
        .map { v => v.initDomain.size.toLong - newState.dom(v).size }
        .sum

      newState

    }

  }

  protected def nextSolution(): SolverResult

  protected def extractSolution(state: ProblemState): Map[Variable, Any] = problem.variables
    .view.map(v => v -> state.dom(v).head)
//    .map {
//    case (variable, dom: IntDomain) => variable -> dom.head
//    case (variable, dom: BooleanDomain) => variable -> dom.canBe(true)
//    case (variable, dom) => throw new NumberFormatException(s"$variable = $dom : $dom should contain integer or boolean values")
//  }
    .toMap

}
