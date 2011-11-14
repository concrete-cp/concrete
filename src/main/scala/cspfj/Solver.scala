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

import cspfj.constraint.extension.MatrixManager2D
import cspfj.constraint.Constraint
import cspfj.constraint.TupleEnumerator
import cspfj.filter.Filter
import cspfj.problem.Problem
import cspfj.util.Parameter
import cspfj.util.Statistic
import cspfj.util.Loggable
import cspfj.util.MsLogHandler
import cspfj.util.Waker
import cspom.CSPOM

object Solver {
  @Parameter("logger.level")
  var loggerLevel = "WARNING";

  @Parameter("solver")
  var solverClass: Class[_ <: Solver] = classOf[MGACIter]

  @Parameter("preprocessor")
  var preprocessorClass: Class[_ <: Filter] = null

  val VERSION = """Rev:\ (\d+)""".r.findFirstIn("$Rev$").get

  ParameterManager.register(this)

  def factory(problem: Problem) = {
    val solver = solverClass.getConstructor(classOf[Problem]).newInstance(problem);
    StatisticsManager.register("solver", solver);
    solver;
  }

  def factory(cspom: CSPOM) = {
    val solver = solverClass.getConstructor(classOf[CSPOM]).newInstance(cspom)
    StatisticsManager.register("solver", solver);
    solver;
  }
}

abstract class Solver(val problem: Problem) extends Loggable {

  @Statistic
  var preproRemoved = 0
  @Statistic
  var preproCpu = 0.0
  @Statistic
  var preproConstraintChecks = 0
  @Statistic
  var preproPresenceChecks = 0
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

    info(ParameterManager.list);
  }

  def nextSolution(): Option[Map[String, Int]]

  private var _maxBacktracks = -1

  var preproExpiration = -1

  final def maxBacktracks = _maxBacktracks

  final def maxBacktracks_=(mBT: Int) {
    _maxBacktracks = mBT
    nbBacktracks = 0
  }

  private var _nbBacktracks = 0

  def nbBacktracks = _nbBacktracks

  @throws(classOf[MaxBacktracksExceededException])
  def nbBacktracks_=(bt: Int) {
    if (bt >= maxBacktracks && maxBacktracks >= 0)
      throw new MaxBacktracksExceededException
    _nbBacktracks = bt
  }

  def reset()

  protected def extractSolution = problem.variables.map { v => v.name -> v.dom.firstValue } toMap

  final def preprocess(filter: Filter): Boolean = {

    info("Preprocessing (" + preproExpiration + ")");

    val preprocessor = if (Solver.preprocessorClass == null) {
      filter
    } else {
      val p = Solver.preprocessorClass.getConstructor(classOf[Problem])
        .newInstance(problem)
      StatisticsManager.register("preprocessor", p)
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
        warning("Interrupted preprocessing");
        true;
        throw e
      }
      case e: OutOfMemoryError => {
        throwing("Filter", "reduceAll", e);
        throw e
      }
    } finally {
      preproCpu += System.currentTimeMillis();
      waker.cancel();

      preproRemoved = problem.variables map { v => v.dom.maxSize - v.dom.size } sum

      this.preproCpu = preproCpu / 1000f;
      preproConstraintChecks = TupleEnumerator.checks
      preproPresenceChecks = Constraint.nbPresenceChecks
      preproMatrix2DChecks = MatrixManager2D.checks
      preproMatrix2DPresenceChecks = MatrixManager2D.presenceChecks
    }

  }

  def XMLConfig = ParameterManager.toXML

}
