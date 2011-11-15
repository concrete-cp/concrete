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

import cspfj.problem.LearnMethod
import cspfj.problem.NoGoodLearner
import cspfj.problem.Variable
import cspfj.Parameter
import cspfj.Statistic
import cspfj.util.Loggable
import cspfj.ParameterManager
import cspfj.StatisticsManager
import cspfj.problem.Problem

/**
 * @author Julien VION
 *
 */
object DC1 {
  @Statistic
  var addedConstraints = 0

  @Statistic
  var noGoods = 0

  @Parameter("dc1.addConstraints")
  val addConstraints = LearnMethod.CONSERVATIVE

  ParameterManager.register(this)
  StatisticsManager.register("DC1", this)
}

final class DC1(val problem: Problem) extends SingletonConsistency with Loggable {

  val ngl = new NoGoodLearner(problem, DC1.addConstraints)

  val subFilter = new AC3(problem)

  override def reduce() = {
    val nbC = problem.constraints.size
    // ExtensionConstraintDynamic.quick = true;

    try {
      super.reduce();
    } finally {
      DC1.addedConstraints += problem.constraints.size - nbC;
    }
  }

  def control() = super.reduce()

  def singletonTest(variable: Variable) = {
    var changedGraph = false;
    for (index <- variable.dom.indices if variable.dom.size > 1) {
      if (Thread.interrupted()) {
        throw new InterruptedException();
      }

      // if (logger.isLoggable(Level.FINER)) {
      // LOGGER.fine(variable + " <- " + variable.getDomain().value(index)
      // + "(" + index + ")");
      // }

      problem.push();
      variable.dom.setSingle(index);

      nbSingletonTests += 1;

      if (subFilter.reduceAfter(variable)) {

        // final Map<Variable[], List<int[]>> noGoods =
        // problem.noGoods();
        val noGoods = ngl.nbNoGoods
        ngl.binNoGoods(variable)
        val newNoGoods = ngl.nbNoGoods - noGoods
        changedGraph |= newNoGoods > 0
        if (newNoGoods > 0) info(newNoGoods + " nogoods");

        problem.pop();

        // changedGraph = problem.noGoodsToConstraints(noGoods,
        // addConstraints);
      } else {
        problem.pop();
        fine("Removing " + variable + ", " + index);

        variable.dom.remove(index);
        changedGraph = true;
      }
    }
    changedGraph;
  }

  override def getStatistics = super.getStatistics ++ Map(
    "CDC-nogoods" -> DC1.noGoods,
    "CDC-added-constraints" -> DC1.addedConstraints)

  override def toString = "DC w/ " + subFilter + " L " + ngl.learnMethod

}
