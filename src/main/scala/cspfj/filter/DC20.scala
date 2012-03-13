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

import scala.annotation.tailrec

import cspfj.constraint.Constraint
import cspfj.constraint.DynamicConstraint
import cspfj.problem.LearnMethod
import cspfj.problem.NoGoodLearner
import cspfj.problem.Problem
import cspfj.problem.Variable
import cspfj.util.Loggable

/**
 * @author Julien VION
 *
 */
final class DC20(val problem: Problem) extends Filter with Loggable {

  val filter = new AC3Constraint(problem)

  private var nbAddedConstraints = 0;

  private var nbNoGoods = 0

  //private var impliedConstraints: Seq[DynamicConstraint] = Nil

  private val modVar = new Array[Int](problem.maxVId + 1)

  private var cnt = 0

  private var nbSingletonTests = 0;
  private val ngl = new NoGoodLearner(problem, LearnMethod.BIN)

  def reduceAll() = {
    val nbC = problem.constraints.size

    //    impliedConstraints = problem.constraints filter (c =>
    //      c.arity == 2 && c.isInstanceOf[DynamicConstraint]) map (_.asInstanceOf[DynamicConstraint])

    // ExtensionConstraintDynamic.quick = true;

    try {
      dcReduce();
    } finally {
      nbAddedConstraints += problem.constraints.size - nbC;
    }

  }

  // private int stId(Variable variable, int value) {
  // return variable.getId() * problem.getMaxDomainSize() + value;
  // }

  private def dcReduce() = filter.reduceAll() && {
    val stream = Stream.continually(problem.variables).flatten

    @tailrec
    def process(variable: Variable, remaining: Stream[Variable], mark: Variable): Boolean = {
      if (mark == variable) true
      else {
        info(variable.toString)
        cnt += 1

        if (singletonTest(variable)) {
          val domainSizes = problem.variables map (_.dom.size)

          if (filter.reduceFrom(modVar, null, cnt - 1)) {
            for (
              v <- problem.variables.iterator.zip(domainSizes.iterator);
              if v._1.dom.size != v._2
            ) {
              fine("Filtered " + (v._2 - v._1.dom.size) + " from " + v._1)
              modVar(v._1.getId) = cnt
            }
            //            if (problem.variables.iterator.zip(domainSizes.iterator) exists {
            //              case (v, d) => v.dom.size != d
            //            }) {
            //              problem.variables.foreach(v => modVar(v.getId) = cnt)
            //            }
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

  private def forwardCheck(variable: Variable) =
    variable.constraints.forall(c => c.arity != 2 || {
      c.setRemovals(variable)
      c.consistentRevise()
    })

  def singletonTest(variable: Variable) = {
    var changedGraph = false;

    for (index <- variable.dom.indices if variable.dom.size > 1) {
      if (Thread.interrupted()) {
        throw new InterruptedException();
      }

      if (logFine) {
        fine(variable + " <- " + variable.dom.value(index) + "(" + index + ")");
      }

      problem.push();
      variable.dom.setSingle(index);

      nbSingletonTests += 1;

      val sat = if (cnt <= problem.variables.size) {
        filter.reduceAfter(variable);
      } else {
        forwardCheck(variable) &&
          filter.reduceFrom(modVar, null, cnt - problem.variables.size);
      }

      if (sat) {
        val noGoods = ngl.nbNoGoods
        val modified = ngl.binNoGoods(variable)
        val newNoGoods = ngl.nbNoGoods - noGoods

        if (newNoGoods > 0) {
          changedGraph = true
          for (v <- modified.flatMap(_.scope)) {
            modVar(v.getId) = cnt
          }
          info(newNoGoods + " nogoods");
        }

        // final Map<Variable[], List<int[]>> noGoods =
        // problem.noGoods();
        //changedGraph = !ngl.binNoGoods(variable).isEmpty || changedGraph;
        // logger.info(noGoods.toString());

        problem.pop();

        // changedGraph = problem.noGoodsToConstraints(noGoods,
        // addConstraints);
      } else {
        problem.pop();
        fine("Removing " + variable + ", " + index);

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
