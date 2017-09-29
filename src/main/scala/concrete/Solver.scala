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

import java.util.Optional

import com.typesafe.scalalogging.LazyLogging
import concrete.constraint.{Constraint, TupleEnumerator}
import concrete.constraint.extension.{BinaryExt, ReduceableExt}
import concrete.constraint.linear.{GtC, LinearLe, LtC}
import concrete.constraint.semantic.DiffN
import concrete.filter.Filter
import concrete.generator.ProblemGenerator
import concrete.generator.cspompatterns.ConcretePatterns
import cspom.{CSPOM, Statistic, StatisticsManager}
import cspom.compiler.CSPOMCompiler
import org.scalameter.Quantity

import scala.collection.JavaConverters._
import scala.util.Try

object Solver {
  def apply(cspom: CSPOM): Try[CSPOMSolver] = apply(cspom, new ParameterManager)

  def apply(cspom: CSPOM, pm: ParameterManager): Try[CSPOMSolver] = {
    CSPOMCompiler.compile(cspom, ConcretePatterns(pm))
      .flatMap { cspom =>
        val pg = new ProblemGenerator(pm)
        pg.generate(cspom)
          .map {
            case (problem, variables) =>
              val solver = apply(problem, pm)
              solver.statistics.register("compiler", CSPOMCompiler)
              solver.statistics.register("generator", pg)

              new CSPOMSolver(solver, cspom.expressionMap, variables)
          }
      }
  }

  def apply(problem: Problem): Solver = apply(problem, new ParameterManager)

  def apply(problem: Problem, pm: ParameterManager): Solver = {
    val solverClass: Class[_ <: Solver] =
      pm.classInPackage("solver", "concrete", classOf[MAC])
    solverClass
      .getMethod("apply", classOf[Problem], classOf[ParameterManager])
      .invoke(null, problem, pm)
      .asInstanceOf[Solver]
  }
}

abstract class Solver(val problem: Problem, val params: ParameterManager) extends Iterator[Map[Variable, Any]] with LazyLogging {

  @Statistic
  val nbConstraints: Int = problem.constraints.length

  implicit class QuantityMath[T](quantity: Quantity[T])(implicit num: Numeric[T]) {
    def +(q: Quantity[T]): Quantity[T] = {
      require(q.units == quantity.units)
      Quantity(num.plus(q.value, quantity.value), q.units)
    }
  }
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
  var preproCpu: Quantity[Double] = _
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
  private var _next: SolverResult = UNKNOWNResult(None)

  def optimises: Option[Variable] = problem.goal.optimizes

  def obtainOptimConstraint[A <: Constraint](f: => A): A = optimConstraint
    .map {
      case c: A@unchecked => c
    }.getOrElse {
    val oc = f
    optimConstraint = Some(oc)
    problem.addConstraint(oc)
    f
  }

  @Statistic
  var nbSolutions = 0

  def next(): Map[Variable, Any] = _next match {
    case UNSAT => Iterator.empty.next
    case UNKNOWNResult(None) => if (hasNext) next() else Iterator.empty.next
    case SAT(sol) =>
      nbSolutions += 1
      _next = UNKNOWNResult(None)
      problem.goal match {
        case Maximize(v) =>
          reset()
          sol(v) match {
            case i: Int =>
              obtainOptimConstraint(new GtC(v, i)).constant = i
              logger.info(s"new best value $i")
            case o => throw new AssertionError(s"$v has value $o which is not an int")
          }

        case Minimize(v) =>
          reset()
          sol(v) match {
            case i: Int =>
              obtainOptimConstraint(new LtC(v, i)).constant = i
              logger.info(s"new best value $i")
            case o => throw new AssertionError(s"$v has value $o which is not an int")
          }

        case Satisfy =>
      }
      assert(problem.constraints.forall(_.positionInVariable.forall(_ >= 0)))
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

  def integerIterator: Iterator[java.util.Map[Variable, java.lang.Integer]] = this.map {
    s =>
      s.map {
        case (v, i: Int) => (v, i: java.lang.Integer)
        case (v, i) => throw new NumberFormatException(s"$v = $i : should be an integer")
      }
        .asJava
  }

  def hasNext = _next match {
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

    val (r, t) = StatisticsManager.measure[Outcome, Unit, Double](preprocessor.reduceAll(state))

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
    .map(v => (v, state.dom(v))).map {
    case (variable, dom: IntDomain) => variable -> dom.head
    case (variable, dom: BooleanDomain) => variable -> dom.canBe(true)
    case (variable, dom) => throw new NumberFormatException(s"$variable = $dom : $dom should contain integer or boolean values")
  }
    .toMap

}

sealed trait SolverResult {
  def isSat: Boolean

  def get: Option[Map[Variable, Any]]

  def getInteger: java.util.Optional[java.util.Map[Variable, java.lang.Integer]] = {
    val o = getInt.map {
      _.mapValues(i => i: java.lang.Integer).asJava
    }

    Optional.ofNullable(o.orNull)
  }

  def getInt: Option[Map[Variable, Int]] = get.map { s =>
    s.mapValues(util.Math.any2Int)
  }
}

case class SAT(val solution: Map[Variable, Any]) extends SolverResult {
  def isSat = true

  def get = Some(solution)

  override def toString = "SAT: " + (if (solution.size > 10) solution.take(10).toString + "..." else solution.toString)
}

case object UNSAT extends SolverResult {
  def isSat = false

  def get = None

  override def toString = "UNSAT"
}

object UNKNOWNResult {
  def apply(cause: Throwable): UNKNOWNResult = UNKNOWNResult(Some(cause))
}

case class UNKNOWNResult(cause: Option[Throwable]) extends SolverResult {
  def isSat = false

  def get = None

  override def toString = "UNKNOWN" + cause
}

case object RESTART extends SolverResult {
  def isSat = false

  def get = None

  override def toString = "RESTART"
}
