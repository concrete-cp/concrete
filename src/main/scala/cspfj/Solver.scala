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

package cspfj;

import java.util.logging.Level
import java.util.logging.Logger
import java.util.Timer
import cspfj.constraint.Constraint
import cspfj.constraint.TupleEnumerator
import cspfj.filter.Filter
import cspfj.problem.Problem
import cspfj.util.Loggable
import cspfj.util.MsLogHandler
import cspfj.util.Waker
import cspfj.generator.ProblemGenerator
import cspom.CSPOM
import cspfj.problem.Variable
import scala.annotation.tailrec
import cspfj.constraint.extension.ExtensionConstraint2D

object Solver {
  @Parameter("logger.level")
  var loggerLevel = "WARNING";

  @Parameter("solver")
  var solverClass: Class[_ <: Solver] = classOf[MAC]

  @Parameter("preprocessor")
  var preprocessorClass: Class[_ <: Filter] = null

  val VERSION = """Rev:\ (\d+)""".r.findFirstMatchIn("$Rev: 830$").get.group(1).toInt

  ParameterManager.register(this)

  def factory(problem: Problem): Solver = {
    val solver = solverClass.getConstructor(classOf[Problem]).newInstance(problem);
    solver;
  }

  def factory(cspom: CSPOM): Solver = factory(ProblemGenerator.generate(cspom))
}

abstract class Solver(val problem: Problem) extends Loggable {

  val statistics = new StatisticsManager
  statistics.register("solver", this)

  @Statistic
  var preproRemoved = 0
  @Statistic
  var preproCpu = 0.0
  @Statistic
  var preproConstraintChecks = 0
  @Statistic
  var preproMatrix2DChecks = 0
  @Statistic
  var preproMatrix2DPresenceChecks = 0

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

  def nextSolution(): SolverResult

  @tailrec
  private def bestSolution(v: Variable, best: SolverResult): SolverResult = {
    nextSolution() match {
      case SAT(sol) =>
        logger.info("New bound " + sol(v.name))
        //if (problem.currentLevel > 0) 
        reset()
        v.dom.removeTo(v.dom.index(sol(v.name)))
        bestSolution(v, SAT(sol))
      case UNSAT => if (best == UNKNOWN) UNSAT else best
      case UNKNOWN => best

    }
  }

  def bestSolution(v: Variable): SolverResult = bestSolution(v, UNKNOWN)

  private var _maxBacktracks = -1

  var preproExpiration = -1

  def reset()

  protected def extractSolution = problem.variables.map { v => v.name -> v.dom.firstValue } toMap

  final def preprocess(filter: Filter): Boolean = {

    logger.info("Preprocessing (" + preproExpiration + ")");

    val preprocessor = if (Solver.preprocessorClass == null) {
      filter
    } else {
      val p = Solver.preprocessorClass.getConstructor(classOf[Problem]).newInstance(problem)
      statistics.register("preprocessor", p)
      p
    }

    Thread.interrupted();

    val waker = new Timer();

    if (preproExpiration >= 0) {
      waker.schedule(new Waker(Thread.currentThread()),
        preproExpiration * 1000);
    }

    var preproCpu = -System.currentTimeMillis();

    try {
      preprocessor.reduceAll();
    } catch {
      case e: InterruptedException => {
        logger.warning("Interrupted preprocessing");
        true;
        throw e
      }
      case e: OutOfMemoryError => {
        logger.throwing("Filter", "reduceAll", e);
        throw e
      }
    } finally {
      preproCpu += System.currentTimeMillis();
      waker.cancel();

      preproRemoved = problem.variables map { v => v.dom.maxSize - v.dom.size } sum

      this.preproCpu = preproCpu / 1000f;
      preproConstraintChecks = TupleEnumerator.checks
      preproMatrix2DChecks = ExtensionConstraint2D.checks
      preproMatrix2DPresenceChecks = ExtensionConstraint2D.presenceChecks
    }

  }

  def XMLConfig = ParameterManager.toXML

}

abstract class SolverResult {
  def isSat: Boolean
  def get: Map[String, Int]
}

case class SAT(val solution: Map[String, Int]) extends SolverResult {
  def isSat = true
  def get = solution
}
case object UNSAT extends SolverResult {
  def isSat = false
  def get = throw new NoSuchElementException
}
case object UNKNOWN extends SolverResult {
  def isSat = false
  def get = throw new NoSuchElementException
}