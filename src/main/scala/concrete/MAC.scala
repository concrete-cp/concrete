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
import cspom.UNSATException

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
      if (problem.variables.isEmpty) 10
      else
        math.max(10, math.log(problem.variables.map(_.initDomain.size).max).toInt)
    } else {
      restartLevel
    }

  var nbBacktracks = 0

  //private var prepared = false

  private var restart = true

  @tailrec
  def mac(
    modifiedVariable: Option[Variable], stack: List[Pair],
    currentState: Outcome, stateStack: List[ProblemState]): (SolverResult, List[Pair], List[ProblemState]) = {
    //if (Thread.interrupted()) throw new InterruptedException()

    val filtering = currentState andThen {
      s =>
        modifiedVariable match {
          case None => s
          case Some(v) =>
            filter.reduceAfter(v, s)

        }
    }

    filtering match {

      case Contradiction =>
        if (stack == Nil) {
          (UNSAT, Nil, Nil)
        } else if (maxBacktracks >= 0 && nbBacktracks >= maxBacktracks) {
          (RESTART, stack, stateStack)
        } else {
          nbBacktracks += 1

          logger.info(s"${stack.length - 1}: ${stack.head} removed")

          val lastAssignment = stack.head

          mac(Some(lastAssignment.variable), stack.tail, stateStack.head.remove(lastAssignment), stateStack.tail)
        }

      case filteredState: ProblemState =>
        heuristic.selectPair(problem, filteredState) match {
          case None =>
            require(problem.variables.forall(v => filteredState.dom(v).size == 1),
              s"Unassigned variables in:\n${problem.variables.map(_.toString(filteredState)).mkString("\n")}")

            for (c <- problem.constraints.find(c => !c.controlAssignment(filteredState))) {
              throw new AssertionError(s"solution does not satisfy ${c.toString(filteredState)}")
            }

            (SAT(extractSolution(filteredState)), stack, filteredState :: stateStack)
          case Some(pair) =>

            logger.info(s"${stack.length}: ${pair.variable.name}: ${filteredState.dom(pair.variable)} <- ${pair.value} ($nbBacktracks / $maxBacktracks)");

            nbAssignments += 1

            val assignedState = filteredState.assign(pair)

            mac(Some(pair.variable), pair :: stack, assignedState, filteredState :: stateStack)

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
  private def nextSolution(modifiedVar: Option[Variable], stack: List[Pair], currentState: Outcome, stateStack: List[ProblemState]): (SolverResult, List[Pair], List[ProblemState]) = {
    logger.info("MAC with " + maxBacktracks + " bt")

    val nbBT = nbBacktracks

    val (macResult, macTime) =
      StatisticsManager.time(mac(modifiedVar, stack, currentState, stateStack))

    searchCpu += macTime

    val (sol, newStack, newStateStack) = macResult.get

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
      nextSolution(None, Nil, newStateStack.last, Nil)
      //      }

    } else { (sol, newStack, newStateStack) }
  }

  var currentStack: List[Pair] = Nil
  var currentStateStack: List[ProblemState] = List(problem.initState.toState)

  def nextSolution(): SolverResult = try {
    // extends state stack for new constraints
    currentStateStack = currentStateStack.map(s =>
      s.padConstraints(problem.constraints, problem.maxCId).toState)

    if (restart) {
      logger.info("RESTART")
      restart = false
      preprocess(filter, currentStateStack.last) match {
        case Contradiction => UNSAT
        case state: ProblemState =>
          val (_, heuristicCpu) = StatisticsManager.time(heuristic.compute(problem))

          val (sol, stack, stateStack) = nextSolution(None, Nil, state, Nil)
          currentStack = stack
          currentStateStack = stateStack
          logger.info(s"Search ended with $sol")
          sol
      }

    } else if (currentStack == Nil) {
      UNSAT
    } else {
      maxBacktracks = -1

      // val nextState = currentStateStack.head.remove(currentStack.head)

      /** Contradiction will trigger a backtrack */

      val (sol, stack, stateStack) =
        nextSolution(None, currentStack, Contradiction, currentStateStack.tail)
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
