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
import cspfj.filter.ACC
import cspfj.filter.Filter
import cspfj.heuristic.CrossHeuristic
import cspfj.heuristic.Heuristic
import cspfj.util.Loggable
import scala.annotation.tailrec

object MAC {
  @Parameter("mac.btGrowth")
  var btGrowth = 1.5;

  @Parameter("mac.addConstraint")
  var addConstraint = LearnMethod.BIN;

  @Parameter("mac.filter")
  var filterClass: Class[_ <: Filter] = classOf[ACC];

  @Parameter("mac.heuristic")
  var heuristicClass: Class[_ <: Heuristic] = classOf[CrossHeuristic];

  @Parameter("mac.restartLevel")
  var restartLevel = 0

  @Parameter("mac.accurateMem")
  var accurateMem = false

  ParameterManager.register(this);

  override def toString = "MAC parameters"
}

final class MAC(prob: Problem) extends Solver(prob) with Loggable {

  @Statistic
  var nbAssignments = 1;

  //private var decisions: List[Pair] = Nil

  private val filter: Filter = MAC.filterClass.getConstructor(classOf[Problem]).newInstance(problem);
  statistics.register("filter", filter);

  private val heuristic: Heuristic = MAC.heuristicClass.getConstructor(classOf[Problem])
    .newInstance(problem);

  private val ngl = new NoGoodLearner(prob, MAC.addConstraint)
  statistics.register("nfr-learner", ngl)

  var maxBacktracks =
    if (MAC.restartLevel == 0) {
      math.max(10, problem.maxDomainSize / 10)
    } else {
      MAC.restartLevel
    }

  var nbBacktracks = 0

  //private var prepared = false

  private var restart = true

  @tailrec
  def mac(modifiedVariable: Option[Variable], stack: List[Pair]): (SolverResult, List[Pair]) = {
    if (Thread.interrupted()) throw new InterruptedException()
    if (modifiedVariable.isEmpty || (
      modifiedVariable.get.dom.size > 0 && filter.reduceAfter(modifiedVariable.get))) {

      heuristic.selectPair(problem) match {
        case None => (SAT(extractSolution), stack)
        case Some(pair) =>

          info(s"${problem.currentLevel}: ${pair.variable} <- ${pair.value} ($nbBacktracks / $maxBacktracks)");

          problem.push()

          nbAssignments += 1

          pair.assign()

          mac(Some(pair.variable), pair :: stack)

      }
    } else if (stack == Nil) {
      (UNSAT, Nil)
    } else if (maxBacktracks >= 0 && nbBacktracks >= maxBacktracks) {
      (RESTART, stack)
    } else {
      problem.pop()
      nbBacktracks += 1

      info(s"${problem.currentLevel}: ${stack.head} removed")
      stack.head.remove()
      mac(Some(stack.head.variable), stack.tail)
    }

  }

  def reset() {
    problem.reset();
    //decisions = Nil
    restart = true
  }

  @Statistic
  var heuristicCpu = 0.0

  @tailrec
  private def nextSolution(modifiedVar: Option[Variable], stack: List[Pair] = Nil): (SolverResult, List[Pair]) = {
    logger.info("MAC with " + maxBacktracks + " bt")

    val nbBT = nbBacktracks

    val ((sol, newStack), macTime) = try {
      StatisticsManager.time(mac(modifiedVar, stack))
    } catch {
      case e: TimedException =>
        searchCpu += e.time;
        throw e.getCause
    }

    searchCpu += macTime

    logger.info("Took " + macTime + "s ("
      + ((nbBacktracks - nbBT) / macTime)
      + " bps)");

    if (sol == RESTART) {

      //val modified = ngl.noGoods(newStack)
      problem.reset();

      //      if (!filter.reduceAfter(modified)) {
      //        (UNSAT, Nil)
      //      } else {
      maxBacktracks = (maxBacktracks * MAC.btGrowth).toInt;
      nbBacktracks = 0
      nextSolution(None)
      //      }

    } else { (sol, newStack) }
  }

  var currentStack: List[Pair] = Nil

  def nextSolution(): SolverResult = try {

    if (restart) {
      restart = false
      if (!preprocess(filter)) {
        UNSAT
      } else {

        val (_, heuristicCpu) = StatisticsManager.time(heuristic.compute())

        val (sol, stack) = nextSolution(None)
        currentStack = stack
        sol
      }

    } else if (currentStack == Nil) {
      UNSAT
    } else {
      maxBacktracks = -1
      problem.pop()
      currentStack.head.remove()

      val (sol, stack) = nextSolution(Some(currentStack.head.variable), currentStack.tail)
      currentStack = stack
      sol
    }

  } finally {
    if (MAC.accurateMem) {
      System.gc()
      System.gc()
      System.gc()
      System.gc()
      System.gc()
    }
    usedMem = math.max(usedMem, Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory())
  }

  @Statistic
  var searchCpu = 0.0

  @Statistic
  var usedMem = 0L

  override def toString =
    "maintain generalized arc consistency - iterative";

}
