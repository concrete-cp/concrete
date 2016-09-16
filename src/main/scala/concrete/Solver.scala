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

import scala.collection.JavaConverters.asJavaIteratorConverter
import scala.collection.JavaConverters.mapAsJavaMapConverter
import scala.collection.JavaConverters.seqAsJavaListConverter
import scala.util.Try
import org.scalameter.Quantity
import com.typesafe.scalalogging.LazyLogging
import concrete.constraint.TupleEnumerator
import concrete.constraint.extension.ReduceableExt
import concrete.constraint.linear.LinearLe
import concrete.constraint.linear.GtC
import concrete.constraint.linear.LtC
import concrete.filter.Filter
import concrete.generator.ProblemGenerator
import concrete.generator.cspompatterns.ConcretePatterns
import concrete.heuristic.BestValue
import cspom.CSPOM
import cspom.Statistic
import cspom.StatisticsManager
import cspom.compiler.CSPOMCompiler
import concrete.constraint.extension.BinaryExt
import concrete.constraint.Constraint
import concrete.constraint.semantic.DiffN

object Solver {
  def apply(cspom: CSPOM): Try[CSPOMSolver] = apply(cspom, new ParameterManager)
  def apply(cspom: CSPOM, pm: ParameterManager): Try[CSPOMSolver] = {
    CSPOMCompiler.compile(cspom, ConcretePatterns(pm))
      .flatMap { cspom =>
        val pg = new ProblemGenerator(pm)
        pg.generate(cspom)
          .flatMap {
            case (problem, variables) =>
              val solver = apply(problem, pm)
              solver.statistics.register("compiler", CSPOMCompiler)
              solver.statistics.register("generator", pg)

              new CSPOMSolver(solver, cspom, variables)
                .applyGoal()
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
  var preproRemoved: Long = -1L

  implicit class QuantityMath[T](quantity: Quantity[T])(implicit num: Numeric[T]) {
    def +(q: Quantity[T]): Quantity[T] = {
      require(q.units == quantity.units)
      Quantity(num.plus(q.value, quantity.value), q.units)
    }
  }

  @Statistic
  var preproCpu: Quantity[Double] = _

  @Statistic
  val nbConstraints = problem.constraints.size

  @Statistic
  val nbVariables = problem.variables.size

  val preprocessorClass: Option[Class[_ <: Filter]] =
    params.get[Class[_ <: Filter]]("preprocessor")

  //  def decisionVariables(dv: Seq[Variable]): Unit = {
  //    logger.info(s"Decision variables: $dv")
  //    problem.decisionVariables = dv.toList
  //    for (v <- optimises) {
  //      if (!problem.decisionVariables.contains(v)) problem.decisionVariables +:= v
  //    }
  //  }

  @Statistic
  val statistics = new StatisticsManager
  //statistics.register("solver", this)
  //statistics.register("domains", IntDomain)
  statistics.register("enumerator", TupleEnumerator)
  statistics.register("relation", ReduceableExt)
  statistics.register("domain", Domain)
  statistics.register("linear", LinearLe)
  statistics.register("binary", BinaryExt)
  statistics.register("diffn", DiffN)

  private var _next: SolverResult = UNKNOWNResult(None)

  private var _minimize: Option[Variable] = None
  private var _maximize: Option[Variable] = None
  def minimize(v: Variable): Solver = {
    _maximize = None; _minimize = Some(v)
    this
  }
  def maximize(v: Variable): Solver = {
    _minimize = None; _maximize = Some(v)
    this
  }

  def optimises: Option[Variable] = _maximize orElse _minimize

  var running = false

  var optimConstraint: Option[Constraint] = None

  def obtainOptimConstraint[A <: Constraint](f: => A): A = optimConstraint
    .map {
      case c: A @unchecked => c
    }.getOrElse {
      val oc = f
      optimConstraint = Some(oc)
      problem.addConstraint(oc)
      f
    }

  def next(): Map[Variable, Any] = _next match {
    case UNSAT => Iterator.empty.next
    case UNKNOWNResult(None) => if (hasNext) next() else Iterator.empty.next
    case SAT(sol) =>
      _next = UNKNOWNResult(None)
      BestValue.newSolution(sol)

      for (v <- _maximize) {
        reset()
        sol(v) match {
          case i: Int => obtainOptimConstraint(new GtC(v, i)).constant = i
          case o => throw new AssertionError(s"$v has value $o which is not an int")
        }

      }
      for (v <- _minimize) {
        reset()
        sol(v) match {
          case i: Int => obtainOptimConstraint(new LtC(v, i)).constant = i
          case o => throw new AssertionError(s"$v has value $o which is not an int")
        }

      }
      assert(problem.constraints.forall(_.positionInVariable.forall(_ >= 0)))
      sol
    case RESTART => throw new IllegalStateException()
  }

  def nextJava(): java.util.Map[Variable, java.lang.Integer] = next()
    .map {
      case (v, i: Int) => (v, i: java.lang.Integer)
    }
    .asJava

  def integerIterator: Iterator[java.util.Map[Variable, java.lang.Integer]] = this.map {
    s =>
      s.map {
        case (v, i: Int) => (v, i: java.lang.Integer)
      }
        .asJava
  }

  def javaIterator: java.util.Iterator[java.util.Map[Variable, java.lang.Integer]] = integerIterator.asJava

  def javaCollection: java.util.List[java.util.Map[Variable, java.lang.Integer]] = integerIterator.toSeq.asJava

  protected def nextSolution(): SolverResult

  def hasNext = _next match {
    case UNSAT => false
    case SAT(_) => true
    case UNKNOWNResult(None) =>
      _next = nextSolution(); hasNext
    case RESTART => throw new IllegalStateException
  }

  private var _maxBacktracks = -1

  def reset()

  protected def extractSolution(state: ProblemState): Map[Variable, Any] = problem.variables
    .map(v => (v, state.dom(v))).map {
      case (variable, dom: IntDomain) => variable -> dom.head
      case (variable, dom: BooleanDomain) => variable -> dom.canBe(true)
    }
    .toMap

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

    val (r, t) = StatisticsManager.measure(preprocessor.reduceAll(state));

    this.preproCpu = t;
    //println(Thread.currentThread().getStackTrace.toSeq)

    r.get.andThen { newState =>
      preproRemoved = problem.variables
        .map { v => v.initDomain.size.toLong - newState.dom(v).size }
        .sum

      newState

    }

  }

}

sealed trait SolverResult {
  def isSat: Boolean
  def get: Map[Variable, Any]
  def getInt: Map[Variable, Int] = get map {
    case (s, false) => (s, 0)
    case (s, true) => (s, 1)
    case (s, i: Int) => (s, i)
  }
  def getInteger: java.util.Map[Variable, java.lang.Integer] =
    get.map {
      case (s, i: Int) => (s, i: java.lang.Integer)
    }
      .asJava
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
object UNKNOWNResult {
  def apply(cause: Throwable): UNKNOWNResult = UNKNOWNResult(Some(cause))
}

case class UNKNOWNResult(cause: Option[Throwable]) extends SolverResult {
  def isSat = false
  def get = throw new NoSuchElementException
  override def toString = "UNKNOWN"
}
case object RESTART extends SolverResult {
  def isSat = false
  def get = throw new NoSuchElementException
  override def toString = "RESTART"
}
