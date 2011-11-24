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
import cspfj.filter.AC3
import cspfj.filter.Filter
import cspfj.heuristic.CrossHeuristic
import cspfj.heuristic.Heuristic
import cspfj.problem.Problem
import cspfj.problem.LearnMethod
import cspfj.problem.NoGoodLearner
import cspfj.problem.Variable
import cspfj.util.Loggable
import scala.annotation.tailrec

object MAC {
  @Parameter("mac.btGrowth")
  var btGrowth = 1.5;

  @Parameter("mac.addConstraint")
  var addConstraint = LearnMethod.BIN;

  @Parameter("mac.filter")
  var filterClass: Class[_ <: Filter] = classOf[AC3];

  @Parameter("mac.heuristic")
  var heuristicClass: Class[_ <: Heuristic] = classOf[CrossHeuristic];

  ParameterManager.register(this);
  StatisticsManager.register("MAC", this);

}

final class MAC(prob: Problem) extends Solver(prob) with Loggable {

  @Statistic
  var nbAssignments = 0;

  private var decisions: List[Pair] = Nil

  private var _filter: Filter = null;

  def filter = _filter

  private var heuristic: Heuristic = null;

  private val ngl = new NoGoodLearner(prob, MAC.addConstraint)
  StatisticsManager.register("nfr-learner", ngl)

  maxBacktracks = math.max(10, problem.maxDomainSize / 10)

  def prepare() {
    if (filter == null) {
      _filter = MAC.filterClass.getConstructor(classOf[Problem]).newInstance(problem);
      StatisticsManager.register("filter", filter);
    }

    if (heuristic == null) {
      heuristic = MAC.heuristicClass.getConstructor(classOf[Problem])
        .newInstance(problem);
    }
  }

  @tailrec
  def mac(modifiedVariable: Variable, stack: List[Pair]): (Option[Map[String, Int]], List[Pair]) = {

    if (modifiedVariable == null || filter.reduceAfter(modifiedVariable)) {

      heuristic.selectPair(problem) match {
        case None => (Some(extractSolution), stack)
        case Some(pair) => {

          info(problem.currentLevel + " : " + pair.variable
            + " <- " + pair.value + "("
            + nbBacktracks + "/" + maxBacktracks + ")");

          problem.push()

          nbAssignments += 1

          pair.variable.dom.setSingle(pair.index)

          mac(pair.variable, pair :: stack)

        }
      }
    } else if (stack == Nil) {
      (None, Nil)
    } else {
      problem.pop()

      nbBacktracks += 1

      stack.head.variable.dom.remove(stack.head.index)
      mac(stack.head.variable, stack.tail)
    }

  }

  def reset() {
    problem.reset();
    decisions = Nil
  }

  @Statistic
  var heuristicCpu = 0.0

  def timedPreprocess() =
    try preprocess(filter)
    catch {
      case _: InterruptedException =>
        filter.reduceAll()
      case e => throw e
    }

  @Override
  def nextSolution(): Option[Map[String, Int]] = {

    prepare();

    // System.gc();

    if (decisions == Nil) {
      if (!timedPreprocess()) {
        return None
      }

      var heuristicCpu = -System.currentTimeMillis();
      heuristic.compute();

      heuristicCpu += System.currentTimeMillis();

      this.heuristicCpu = heuristicCpu / 1000f;

    } else {
      maxBacktracks = -1
      problem.pop()
      val ld = decisions.head
      decisions = decisions.tail
      ld.variable.dom.remove(ld.index)
    }

    var solution: Option[Map[String, Int]] = null

    while (solution == null) {

      info("MAC with " + maxBacktracks + " bt")
      var macTime = -System.currentTimeMillis()
      val nbBT = nbBacktracks

      try {
        val (solution, newDecisions) = mac(null, decisions)
        decisions = newDecisions
      } catch {
        case e: MaxBacktracksExceededException => {
          val modified = ngl.noGoods(decisions)
          problem.reset();
          decisions = Nil

          if (!filter.reduceAfter(modified)) {
            solution = None
          }

          maxBacktracks = (maxBacktracks * MAC.btGrowth).toInt;
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
