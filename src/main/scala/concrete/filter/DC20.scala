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
package concrete.filter;

import scala.annotation.tailrec
import concrete.constraint.Constraint
import concrete.LearnMethod
import concrete.NoGoodLearner
import concrete.Problem
import concrete.Variable
import concrete.util.Loggable
import cspom.Statistic
import concrete.UNSATException
import concrete.LearnBin

/**
 * @author Julien VION
 *
 */
final class DC20(val problem: Problem) extends Filter with Loggable {

  val filter = new ACC(problem)

  @Statistic
  var nbAddedConstraints = 0;

  private var nbNoGoods = 0

  //private var impliedConstraints: Seq[DynamicConstraint] = Nil

  private val modVar = new Array[Int](problem.nbVariables)

  private var cnt = 0

  private var nbSingletonTests = 0;
  private val ngl = new NoGoodLearner(problem, LearnBin)

  val nbVar = problem.variables.size

  def reduceAll() = {
    val nbC = problem.constraints.size

    try {
      filter.reduceAll() && dcReduce(Stream.continually(problem.variables).flatten, None)
    } finally {
      nbAddedConstraints += problem.constraints.size - nbC;
    }

  }

  // private int stId(Variable variable, int value) {
  // return variable.getId() * problem.getMaxDomainSize() + value;
  // }

  @tailrec
  private def dcReduce(s: Stream[Variable], mark: Option[Variable]): Boolean = {
    val variable #:: remaining = s
    if (mark == Some(variable)) { true }
    else {
      logger.info(variable.toString)
      cnt += 1

      if (singletonTest(variable)) {
        val domainSizes = problem.variables map (_.dom.size)

        if (filter.reduceFrom(modVar, null, cnt - 1)) {
          for (
            (v, od) <- problem.variables.zip(domainSizes);
            if v.dom.size != od
          ) {
            logger.info("Filtered " + (od - v.dom.size) + " from " + v)
            modVar(v.getId) = cnt
          }

          dcReduce(remaining, Some(variable))
        } else { false }

      } else {
        dcReduce(remaining, mark.orElse(Some(variable)))
      }
    }
  }

  private def forwardCheck(variable: Variable) =
    variable.constraints.forall { c =>
      c.advise(variable)
      c.arity != 2 || (try {
        c.revise()
        true
      } catch {
        case _: UNSATException => false
      })
    }

  def singletonTest(variable: Variable) = {
    var changedGraph = false;

    for (index <- variable.dom.indices if variable.dom.size > 1) {
      if (Thread.interrupted()) {
        throw new InterruptedException();
      }

      if (logFine) {
        logger.fine(variable + " <- " + variable.dom.value(index) + " (" + index + ")");
      }

      problem.push();
      variable.dom.setSingle(index);

      nbSingletonTests += 1;

      val sat = if (cnt <= nbVar) {
        filter.reduceAfter(variable);
      } else {
        forwardCheck(variable) &&
          filter.reduceFrom(modVar, null, cnt - nbVar);
      }

      if (sat) {
        val noGoods = ngl.nbNoGoods
        val modified = ngl.binNoGoods(variable)

        if (modified.nonEmpty) {
          changedGraph = true
          for (v <- modified.flatMap(_.scope)) {
            modVar(v.getId) = cnt
          }
          logger.info((ngl.nbNoGoods - noGoods) + " nogoods");
        }

        problem.pop();

      } else {
        problem.pop();
        logger.fine("Removing " + variable + ", " + index);

        variable.dom.remove(index);
        modVar(variable.getId) = cnt
        changedGraph = true;
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
      reduceAll();
    } catch {
      case e: InterruptedException =>
        throw new IllegalStateException(
          "Filter was unexpectingly interrupted !", e);
    }
  }

  def reduceAfter(constraints: Iterable[Constraint]) =
    throw new UnsupportedOperationException();

}
