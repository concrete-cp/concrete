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

import java.lang.Override

import scala.collection.JavaConversions

import cspfj.filter.AC3Constraint
import cspfj.filter.Filter
import cspfj.heuristic.CrossHeuristic
import cspfj.heuristic.Heuristic
import cspfj.problem.Problem
import cspfj.problem.LearnMethod
import cspfj.problem.NoGoodLearner
import cspfj.problem.Variable
import cspfj.util.Parameter
import cspfj.util.Statistic
import cspfj.util.Loggable

object MGACIter {
  @Parameter("mgac.btGrowth")
  var btGrowth = 1.5;

  @Parameter("mgac.addConstraint")
  var addConstraint = LearnMethod.BIN;

  @Parameter("mgac.filter")
  var filterClass: Class[_ <: Filter] = classOf[AC3Constraint];

  @Parameter("mgac.heuristic")
  var heuristicClass: Class[_ <: Heuristic] = classOf[CrossHeuristic];

  ParameterManager.register(this);
  StatisticsManager.register("MGACIter", this);
}

final class MGACIter(prob: Problem) extends Solver(prob) with Loggable {

  @Statistic
  var nbAssignments = 0;

  private var decisions: List[Pair] = Nil

  private var _filter: Filter = null;

  def filter = _filter

  private var heuristic: Heuristic = null;

  private val ngl = new NoGoodLearner(prob, MGACIter.addConstraint)
  StatisticsManager.register("nfr-learner", ngl)

  maxBacktracks = math.max(10, problem.maxDomainSize / 10)

  private def prepare() {
    if (filter == null) {
      _filter = MGACIter.filterClass.getConstructor(classOf[Problem]).newInstance(
        problem);
      StatisticsManager.register("filter", filter);
    }

    if (heuristic == null) {
      heuristic = MGACIter.heuristicClass.getConstructor(classOf[Problem])
        .newInstance(problem);
    }
  }

  private var firstSolutionGiven = false;

  def mac(skipFirstSolution: Boolean): Option[Map[String, Int]] = {
    var skipSolution = skipFirstSolution;
    var selectedVariable: Variable = null;
    var selectedIndex = -1;
    var solution: Option[Map[String, Int]] = null
    while (solution == null) {
      if (selectedVariable != null
        && !filter.reduceAfter(selectedVariable)) {
        selectedVariable = backtrack();
        if (selectedVariable == null) {
          solution = None
        }
      } else {

        heuristic.selectPair(problem) match {
          case None => if (skipSolution) {
            selectedVariable = backtrack();
            if (selectedVariable == null) {
              solution = None
            }
            skipSolution = false;
          } else {
            solution = Some(extractSolution)
          }
          case Some(pair) => {
            decisions ::= pair
            selectedVariable = pair.variable

            assert(selectedVariable.dom.size > 0)

            selectedIndex = pair.index

            assert(selectedVariable.dom.present(selectedIndex))
            //
            fine(problem.currentLevel + " : " + selectedVariable
              + " <- "
              + selectedVariable.dom.value(selectedIndex) + "("
              + nbBacktracks + "/" + maxBacktracks + ")");

            problem.push();
            selectedVariable.dom.setSingle(selectedIndex);
            nbAssignments += 1;
          }
        }

      }

    }
    solution

  }

  private def backtrack(): Variable = {
    var decision: Pair = null;
    do {
      if (decisions == Nil) {
        return null;
      }
      decision = decisions.head;
      decisions = decisions.tail
      // decision.getVariable().unassign();
      problem.pop();

      // LOGGER.finer(problem.getCurrentLevel() + " : "
      // + decision.getVariable() + " /= "
      // + decision.getVariable().getValue(decision.getIndex()));
    } while (decision.variable.dom.size <= 1);

    decision.variable.dom.remove(decision.index);
    nbBacktracks += 1;
    return decision.variable;
  }

  def reset() {
    firstSolutionGiven = false;
    problem.reset();
    decisions = Nil
  }

  @Statistic
  var heuristicCpu = 0.0

  @Override
  def nextSolution(): Option[Map[String, Int]] = {

    prepare();

    // System.gc();

    if (firstSolutionGiven) {
      maxBacktracks = -1
    } else {
      try {
        if (!preprocess(filter)) {
          firstSolutionGiven = true;
          return None;
        }
      } catch {
        case e: InterruptedException =>
          if (!filter.reduceAll()) {
            return None;
          }
      }

      var heuristicCpu = -System.currentTimeMillis();
      heuristic.compute();

      heuristicCpu += System.currentTimeMillis();

      this.heuristicCpu = heuristicCpu / 1000f;

    }

    var solution: Option[Map[String, Int]] = null

    while (solution == null) {

      info("MAC with " + maxBacktracks + " bt")
      var macTime = -System.currentTimeMillis()
      var nbBT = nbBacktracks

      try {
        solution = mac(firstSolutionGiven)
        firstSolutionGiven = true;
      } catch {
        case e: MaxBacktracksExceededException => {
          val modified = ngl.noGoods(decisions)
          problem.reset();
          decisions = Nil

          if (!filter.reduceAfter(modified)) {
            solution = None
          }

          maxBacktracks = (maxBacktracks * MGACIter.btGrowth).toInt;
        }
      } finally {
        macTime += System.currentTimeMillis();
        searchCpu += macTime / 1000f;
        info("Took " + (macTime / 1000f) + "s ("
          + (1000f * (nbBacktracks - nbBT) / macTime)
          + " bps)");
      }

    }

    solution;

  }

  @Statistic
  var searchCpu = 0.0

  override def toString =
    "maintain generalized arc consistency - iterative";

}
