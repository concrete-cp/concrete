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

import cspfj.problem.Problem
import cspfj.util.Parameter
import cspfj.filter.Filter
import cspom.CSPOM
import cspfj.exception.MaxBacktracksExceededException
import java.util.logging.Level
import java.util.logging.Logger
import cspfj.util.MsLogHandler
import cspfj.util.Loggable

object Solver {
  @Parameter("logger.level")
  var loggerLevel = "WARNING";

  @Parameter("solver")
  var solverClass: Class[_ <: Solver] = classOf[MGACIter]

  @Parameter("preprocessor")
  var preprocessorClass: Class[_ <: Filter] = null

  val VERSION = """Rev:\\ (\\d+)""".r.findFirstIn("$Rev: -1 $").get

  ParameterManager.register(Solver)

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

trait Solver extends Traversable[Map[String, Int]] with Loggable {
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

    info(ParameterManager.list());
  }

  def nextSolution(): Map[String, Int]

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

  def xmlConfig: xml.Elem

  def problem: Problem

  def preproExp_=(time: Int)

  def reset()

}