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
import cspfj.constraint.DynamicConstraint
import cspfj.problem.LearnMethod
import cspfj.problem.NoGoodLearner
import cspfj.problem.Problem
import cspfj.problem.Variable
import cspfj.Parameter
import cspfj.util.Loggable
import scala.annotation.tailrec

object DC20 {
  @Parameter("dc20.addConstraints")
  var addConstraints = LearnMethod.CONSERVATIVE;

  ParameterManager.register(this);

}

/**
 * @author Julien VION
 *
 */
final class DC20(val problem: Problem) extends Filter with Loggable {

  val filter = new AC3(problem)

  private var nbAddedConstraints = 0;

  private var nbNoGoods = 0

  private var impliedConstraints: Seq[DynamicConstraint] = Nil

  private val modVar = new Array[Int](problem.maxVId + 1)

  private var cnt = 0

  private var nbSingletonTests = 0;
  private val ngl = new NoGoodLearner(problem, DC20.addConstraints)

  def reduceAll() = {
    val nbC = problem.constraints.size

    impliedConstraints = problem.constraints filter (c =>
      c.arity == 2 && c.isInstanceOf[DynamicConstraint]) map (_.asInstanceOf[DynamicConstraint])

    // ExtensionConstraintDynamic.quick = true;

    try {
      cdcReduce();
    } finally {
      nbAddedConstraints += problem.constraints.size - nbC;
    }

  }

  // private int stId(Variable variable, int value) {
  // return variable.getId() * problem.getMaxDomainSize() + value;
  // }

  private def cdcReduce() = {
    if (!filter.reduceAll()) {
      false;
    } else {
      val stream = Stream.continually(problem.variables.toStream).flatten

      @tailrec
      def process(variable: Variable, remaining: Stream[Variable], mark: Variable): Boolean = {
        if (mark == variable) {
          true
        } else {
          info(variable.toString)

          if (variable.dom.size > 1 && singletonTest(variable)) {
            val domainSizes = problem.variables map (_.dom.size)

            if (filter.reduceFrom(modVar, null, cnt - 1)) {
              for (v <- problem.variables if domainSizes(v.getId) != v.dom.size) {
                modVar(v.getId) = cnt
              }
              process(remaining.head, remaining.tail, variable)
            } else {
              false
            }
          } else {
            process(remaining.head, remaining.tail, if (mark == null) variable else mark)
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
      fine(variable + " <- " + variable.dom.value(index) + "(" + index + ")");
      // }

      problem.push();
      variable.dom.setSingle(index);

      nbSingletonTests += 1;

      val sat =

        if (cnt <= problem.variables.size) {
          filter.reduceAfter(variable);
        } else {
          for (c <- variable.constraints if c.arity == 2) {
            c.revise(rh, -1);
            c.fillRemovals(-1);
          }

          filter.reduceFrom(modVar, null, cnt - problem.variables.size);
        }
      if (sat) {

        // final Map<Variable[], List<int[]>> noGoods =
        // problem.noGoods();
        changedGraph = !ngl.binNoGoods(variable).isEmpty || changedGraph;
        // logger.info(noGoods.toString());

        problem.pop();

        // changedGraph = problem.noGoodsToConstraints(noGoods,
        // addConstraints);
      } else {
        problem.pop();
        fine("Removing " + variable + ", " + index);

        variable.dom.remove(index);
        changedGraph = true;
        modVar(variable.getId) = cnt;
      }
    }

    changedGraph;
  }

  private val rh = new RevisionHandler() {
    def revised(constraint: Constraint, variable: Variable) {
      //
    }
  };

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
    } else
      try {
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
