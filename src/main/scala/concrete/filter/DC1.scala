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

import concrete.LearnMethod
import concrete.NoGoodLearner
import concrete.Variable
import concrete.Parameter
import cspom.Statistic
import com.typesafe.scalalogging.slf4j.LazyLogging
import concrete.ParameterManager
import cspom.StatisticsManager
import concrete.Problem

/**
 * @author Julien VION
 *
 */
final class DC1(val problem: Problem, params: ParameterManager) extends SingletonConsistency with LazyLogging {
  @Parameter("dc1.addConstraints")
  var addConstraints = "CONSERVATIVE"

  params.register(this)

  @Statistic
  var addedConstraints = 0

  @Statistic
  var noGoods = 0

  val ngl = new NoGoodLearner(problem, LearnMethod(addConstraints))

  val subFilter = new ACC(problem, params)

  override def reduce() = {
    val nbC = problem.constraints.size
    // ExtensionConstraintDynamic.quick = true;

    try {
      super.reduce();
    } finally {
      addedConstraints += problem.constraints.size - nbC;
    }
  }

  def control() = super.reduce()

  def singletonTest(variable: Variable) = {
    var changedGraph = false;
    for (index <- variable.dom.indices if variable.dom.size > 1) {
      if (Thread.interrupted()) {
        throw new InterruptedException();
      }

      logger.debug(variable + " <- " + variable.dom.value(index)
        + " (" + index + ")");

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
        if (newNoGoods > 0) logger.info(newNoGoods + " nogoods");

        problem.pop();

        // changedGraph = problem.noGoodsToConstraints(noGoods,
        // addConstraints);
      } else {
        problem.pop();
        logger.debug("Removing " + variable + ", " + index);

        variable.dom.remove(index);
        changedGraph = true;
      }
    }
    changedGraph;
  }

  override def getStatistics = super.getStatistics ++ Map(
    "CDC-nogoods" -> noGoods,
    "CDC-added-constraints" -> addedConstraints)

  override def toString = "DC w/ " + subFilter + " L " + ngl.learnMethod

}
