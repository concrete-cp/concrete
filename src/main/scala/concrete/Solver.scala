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

import java.util.logging.Level
import java.util.logging.Logger
import scala.annotation.tailrec
import scala.collection.JavaConversions
import concrete.filter.Filter
import concrete.generator.ProblemGenerator
import concrete.util.Loggable
import concrete.util.MsLogHandler
import concrete.util.Waker
import concrete.constraint.extension.MDD
import cspom.CSPOM
import concrete.constraint.TupleEnumerator
import concrete.constraint.extension.ReduceableExt

object Solver {
  @Parameter("logger.level")
  var loggerLevel = "WARNING";

  @Parameter("solver")
  var solverClass: Class[_ <: Solver] = classOf[MAC]

  @Parameter("preprocessor")
  var preprocessorClass: Class[_ <: Filter] = null

  val VERSION = s"Concrete 2.0-SNAPSHOT"

  ParameterManager.register(this)

  def apply(problem: Problem): Solver = {
    val solver = solverClass.getConstructor(classOf[Problem]).newInstance(problem);
    solver;
  }

  def apply(cspom: CSPOM): Solver = apply(ProblemGenerator.generate(cspom))
}

abstract class Solver(val problem: Problem) extends Iterator[Map[String, Int]] with Loggable {

  @Statistic
  var preproRemoved = 0
  @Statistic
  var preproCpu = 0.0

  val statistics = new StatisticsManager
  statistics.register("solver", this)
  //statistics.register("domains", IntDomain)
  statistics.register("enumerator", TupleEnumerator)
  statistics.register("relation", ReduceableExt)
  statistics.register("domain", Domain)

  /** Logger initialization */
  {
    val level = Level.parse(Solver.loggerLevel);

    Logger.getLogger("").setLevel(level);
    for (h <- Logger.getLogger("").getHandlers()) {
      Logger.getLogger("").removeHandler(h);
    }

    val handler = new MsLogHandler(System.currentTimeMillis());
    handler.setLevel(level);
    Logger.getLogger("").addHandler(handler);

    logger.info(ParameterManager.list);
  }

  private var _next: SolverResult = UNKNOWNResult
  private var _minimize: Option[Variable] = None
  private var _maximize: Option[Variable] = None
  def minimize(v: Variable) { _maximize = None; _minimize = Some(v) }
  def maximize(v: Variable) { _minimize = None; _maximize = Some(v) }

  def next() = _next match {
    case UNSAT => Iterator.empty.next
    case UNKNOWNResult => if (hasNext()) next() else Iterator.empty.next
    case SAT(sol) =>
      _next = UNKNOWNResult
      for (v <- _maximize) {
        reset()
        v.dom.removeTo(v.dom.index(sol(v.name)))
      }
      for (v <- _minimize) {
        reset()
        v.dom.removeFrom(v.dom.index(sol(v.name)))
      }
      sol
    case RESTART => throw new IllegalStateException()
  }

  protected def nextSolution(): SolverResult

  def hasNext() = _next match {
    case UNSAT => false
    case SAT(_) => true
    case UNKNOWNResult =>
      _next = nextSolution(); hasNext()
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

  protected def extractSolution = problem.variables.map { v => v.name -> v.dom.firstValue } toMap

  final def preprocess(filter: Filter): Boolean = {

    logger.info("Preprocessing");

    val preprocessor = if (Solver.preprocessorClass == null) {
      filter
    } else {
      val p = Solver.preprocessorClass.getConstructor(classOf[Problem]).newInstance(problem)
      statistics.register("preprocessor", p)
      p
    }

    val (r, t) = StatisticsManager.time(preprocessor.reduceAll());

    preproRemoved = problem.variables map { v => v.dom.maxSize - v.dom.size } sum

    this.preproCpu = t;
    r

  }

  def xmlConfig = ParameterManager.toXML

}

sealed trait SolverResult {
  def isSat: Boolean
  def get: Map[String, Int]
  def getNum: Map[String, Number] = get map {
    case (s, i) => (s, i.asInstanceOf[Number])
  }
  def getInteger: java.util.Map[String, java.lang.Integer] = JavaConversions.mapAsJavaMap(get map {
    case (s, i) => (s, i.asInstanceOf[java.lang.Integer])
  })
}

case class SAT(val solution: Map[String, Int]) extends SolverResult {
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
