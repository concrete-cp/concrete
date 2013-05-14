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
package cspfj.filter;

import scala.collection.IndexedSeq
import scala.collection.JavaConversions
import cspfj.ParameterManager
import cspfj.constraint.Constraint
import cspfj.LearnMethod
import cspfj.NoGoodLearner
import cspfj.Problem
import cspfj.Variable
import cspfj.Parameter
import cspfj.util.Loggable
import scala.annotation.tailrec

/**
 * @author Julien VION
 *
 */
final class DC21(val problem: Problem) extends Filter with Loggable {

  private var nbAddedConstraints = 0;

  private var nbNoGoods = 0

  private val filter = new ACV(problem)

  // private final List<DynamicConstraint> impliedConstraints;

  private val modVar = new Array[Int](problem.maxVId + 1)
  private val modCons = new Array[Int](problem.maxCId + 1)

  private var cnt = 0

  private var nbSingletonTests = 0;

  private val ngl = new NoGoodLearner(problem, LearnMethod.BIN);

  def reduceAll() = {
    val nbC = problem.constraints.size

    try {
      dcReduce();
    } finally {
      nbAddedConstraints += problem.constraints.size - nbC;
    }
  }

  private def dcReduce() = {
    if (!filter.reduceAll()) {
      false;
    } else {
      val stream = Stream.continually(problem.variables.toStream).flatten

      @tailrec
      def process(variable: Variable, remaining: Stream[Variable], mark: Option[Variable]): Boolean = {
        if (mark == Some(variable)) {
          true
        } else {
          logger.info(variable.toString)
          cnt += 1
          if (variable.dom.size > 1 && singletonTest(variable)) {
            val domainSizes = problem.variables map { _.dom.size }
            if (filter.reduceFrom(modVar, modCons, cnt - 1)) {
              for (v <- problem.variables if domainSizes(v.getId) != v.dom.size) {
                modVar(v.getId) = cnt;
              }

              process(remaining.head, remaining.tail, Some(variable))
            } else {
              false
            }

          } else {
            process(remaining.head, remaining.tail, mark.orElse(Some(variable)))
          }
        }
      }

      process(stream.head, stream.tail, null)

    }
  }

  def singletonTest(variable: Variable) = {
    var changedGraph = false;

    for (index <- variable.dom.indices) {
      if (Thread.interrupted()) {
        throw new InterruptedException();
      }

      // if (logger.isLoggable(Level.FINER)) {
      // LOGGER.fine(variable + " <- " +
      // variable.getDomain().value(index));
      // }

      problem.push();
      variable.dom.setSingle(index);

      nbSingletonTests += 1;

      val sat = if (cnt <= problem.variables.size) {
        filter.reduceAfter(variable);
      } else {
        /**
         * Forward checking !
         */
        for (c <- variable.constraints if c.arity == 2) {
          (0 until c.arity).foreach(c.advise)
          c.revise();
        }

        filter.reduceFrom(modVar, modCons, cnt - problem.variables.size);
      }
      if (sat) {

        val modified = ngl.binNoGoods(variable)

        if (!modified.isEmpty) {
          changedGraph = true;

          for (c <- modified) {
            modCons(c.getId) = cnt;
          }
        }

        problem.pop();

        // changedGraph = problem.noGoodsToConstraints(noGoods,
        // addConstraints);
      } else {
        problem.pop();
        logger.fine("Removing " + variable + ", " + index);

        variable.dom.remove(index);
        changedGraph = true;
        modVar(variable.getId) = cnt;
      }
    }
    changedGraph;
  }

  def getStatistics = Map(
    "DC-nbsingletontests" -> nbSingletonTests,
    "DC-nogoods" -> nbNoGoods,
    "DC-added-constraints" -> nbAddedConstraints) ++
    filter.getStatistics map {
      case (k, v) =>
        "DC-backend-" + k -> v
    }

  override def toString = "DC20 w/ " + filter + " L " + ngl.learnMethod

  def reduceAfter(variable: Variable) = {
    if (variable == null) {
      true;
    } else try {
      reduceAll()
    } catch {
      case e: InterruptedException =>
        throw new IllegalStateException(
          "Filter was unexpectingly interrupted !", e);
    }
  }

  def reduceAfter(constraints: Iterable[Constraint]) =
    throw new UnsupportedOperationException();

}
