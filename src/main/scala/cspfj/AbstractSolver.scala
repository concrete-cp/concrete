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

import java.lang.reflect.InvocationTargetException;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Timer;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import cspfj.constraint.Constraint;
import cspfj.constraint.TupleEnumerator;
import cspfj.constraint.extension.MatrixManager2D;
import cspfj.exception.MaxBacktracksExceededException;
import cspfj.filter.Filter;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspfj.util.MsLogHandler;
import cspfj.util.Parameter;
import cspfj.util.Statistic;
import cspfj.util.Waker;
import cspom.CSPOM;

abstract class AbstractSolver(val problem: Problem) extends Solver {

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
