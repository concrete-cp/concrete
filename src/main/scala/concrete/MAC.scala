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

package concrete;

import java.lang.Override
import scala.collection.JavaConversions
import concrete.filter.ACC
import concrete.filter.Filter
import concrete.heuristic.CrossHeuristic
import concrete.heuristic.Heuristic
import com.typesafe.scalalogging.LazyLogging
import cspom.Statistic
import cspom.StatisticsManager
import scala.annotation.tailrec
import cspom.TimedException

final class MAC(prob: Problem, params: ParameterManager) extends Solver(prob, params) with LazyLogging {

  val btGrowth: Double = params.getOrElse("mac.btGrowth", 1.5)

  //  def addConstraint: LearnMethod = LearnMethod(
  //    params.getOrElse("mac.addConstraint", "BIN"))

  val filterClass: Class[_ <: Filter] =
    params.getOrElse("mac.filter", classOf[ACC])

  val heuristicClass: Class[_ <: Heuristic] =
    params.getOrElse("mac.heuristic", classOf[CrossHeuristic])

  val restartLevel =
    params.getOrElse("mac.restartLevel", 0)

  val measureMem: Boolean =
    params.getOrElse("mac.measureMem", false)

  @Statistic
  var nbAssignments = 1;

  //def this(p: Problem) = this(p, new ParameterManager())

  //private var decisions: List[Pair] = Nil

  private val filter: Filter = filterClass.getConstructor(classOf[Problem], classOf[ParameterManager]).newInstance(problem, params);
  statistics.register("filter", filter);

  var heuristic: Heuristic = heuristicClass.getConstructor(classOf[ParameterManager]).newInstance(params);

  //  private val ngl = new NoGoodLearner(prob, addConstraint)
  //  statistics.register("nfr-learner", ngl)

  var maxBacktracks: Int =
    if (restartLevel == 0) {
      math.max(10, math.log(problem.variables.map(_.initDomain.size).max).toInt)
    } else {
      restartLevel
    }

  var nbBacktracks = 0

  //private var prepared = false

  private var restart = true

  @tailrec
  def mac(modifiedVariable: Option[Variable], stack: List[Pair], stateStack: List[ProblemState]): (SolverResult, List[Pair], List[ProblemState]) = {
    if (Thread.interrupted()) throw new InterruptedException()

    val currentState = stateStack.head

    val filtering = modifiedVariable match {
      case None => Filtered(currentState)
      case Some(v) =>
        if (currentState(v).isEmpty) {
          Contradiction
        } else {
          filter.reduceAfter(v, currentState)
        }
    }

    filtering match {
      case Filtered(filteredState) =>
        heuristic.selectPair(problem, filteredState) match {
          case None => (SAT(extractSolution(filteredState)), stack, filteredState :: stateStack.tail)
          case Some(pair) =>

            logger.info(s"${stack.length}: ${pair.variable.name}: ${filteredState(pair.variable)} <- ${pair.value} ($nbBacktracks / $maxBacktracks)");

            nbAssignments += 1

            val assignedState = filteredState.assign(pair)

            mac(Some(pair.variable), pair :: stack, assignedState :: filteredState :: stateStack.tail)

        }
      case Contradiction =>
        if (stack == Nil) {
          (UNSAT, Nil, Nil)
        } else if (maxBacktracks >= 0 && nbBacktracks >= maxBacktracks) {
          (RESTART, stack, stateStack)
        } else {
          nbBacktracks += 1

          logger.info(s"${stack.length}: ${stack.head} removed")

          val bt = stateStack.tail
          val last = stack.head

          mac(Some(last.variable), stack.tail, bt.head.remove(last) :: bt.tail)
        }
    }

  }

  def reset() {
    //decisions = Nil
    restart = true
  }

  @Statistic
  var heuristicCpu = 0.0

  @tailrec
  private def nextSolution(modifiedVar: Option[Variable], stack: List[Pair] = Nil, stateStack: List[ProblemState]): (SolverResult, List[Pair], List[ProblemState]) = {
    logger.info("MAC with " + maxBacktracks + " bt")

    val nbBT = nbBacktracks

    val ((sol, newStack, newStateStack), macTime) = try {
      StatisticsManager.time(mac(modifiedVar, stack, stateStack))
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

      //      if (!filter.reduceAfter(modified)) {
      //        (UNSAT, Nil)
      //      } else {
      maxBacktracks = (maxBacktracks * btGrowth).toInt;
      nbBacktracks = 0
      nextSolution(None, Nil, List(newStateStack.last))
      //      }

    } else { (sol, newStack, newStateStack) }
  }

  var currentStack: List[Pair] = Nil
  var currentStateStack: List[ProblemState] = List(problem.initState)

  def nextSolution(): SolverResult = try {

    if (restart) {
      restart = false
      preprocess(filter, currentStateStack.last) match {
        case Contradiction => UNSAT
        case Filtered(state) =>
          val (_, heuristicCpu) = StatisticsManager.time(heuristic.compute(problem))

          val (sol, stack, stateStack) = nextSolution(None, Nil, List(state))
          currentStack = stack
          currentStateStack = stateStack
          sol
      }

    } else if (currentStack == Nil) {
      UNSAT
    } else {
      maxBacktracks = -1

      val bt = currentStateStack.tail

      val nextStateStack = bt.head.remove(currentStack.head) :: bt.tail

      val (sol, stack, stateStack) = nextSolution(Some(currentStack.head.variable), currentStack.tail, nextStateStack)
      currentStack = stack
      currentStateStack = stateStack
      sol
    }

  } finally {
    if (measureMem || usedMem == 0L) {
      System.gc()
      System.gc()
      System.gc()
      System.gc()
      System.gc()
      usedMem = math.max(usedMem, Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory())
    }
  }

  @Statistic
  var searchCpu = 0.0

  @Statistic
  var usedMem = 0L

  override def toString =
    "maintain generalized arc consistency - iterative";

}
