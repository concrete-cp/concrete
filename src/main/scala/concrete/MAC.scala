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
import concrete.heuristic.Branch
import org.scalameter.Quantity
import org.scalameter.Key
import org.scalameter.Warmer

object MAC {
  def apply(prob: Problem, params: ParameterManager): MAC = {
    val heuristic: Heuristic = params.get[Any]("mac.heuristic").getOrElse(classOf[CrossHeuristic]) match {
      case heuristicClass: Class[_] =>
        heuristicClass
          .getMethod("apply", classOf[ParameterManager], classOf[List[Variable]])
          .invoke(null, params, prob.variables)
          .asInstanceOf[Heuristic]
      case heuristic: Heuristic => heuristic
    }

    new MAC(prob, params, heuristic)
  }
}

final class MAC(prob: Problem, params: ParameterManager, val heuristic: Heuristic) extends Solver(prob, params) with LazyLogging {
  //println(prob.toString(problem.initState.toState))
  val btGrowth: Double = params.getOrElse("mac.btGrowth", 1.5)

  //  def addConstraint: LearnMethod = LearnMethod(
  //    params.getOrElse("mac.addConstraint", "BIN"))

  val filterClass: Class[_ <: Filter] = params.getOrElse("mac.filter", classOf[ACC])

  val restartLevel = params.getOrElse("mac.restartLevel", if (heuristic.shouldRestart) 0 else -1)

  val measureMem: Boolean = params.getOrElse("mac.measureMem", false)

  @Statistic
  var nbAssignments = 1;

  //def this(p: Problem) = this(p, new ParameterManager())

  //private var decisions: List[Pair] = Nil

  private val filter: Filter = filterClass.getConstructor(classOf[Problem], classOf[ParameterManager]).newInstance(problem, params);
  statistics.register("filter", filter);

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

  //private var prepared = false

  private var restart = true

  @tailrec
  def mac(
    modifiedVariable: Seq[Variable], stack: List[Branch],
    currentState: Outcome, stateStack: List[ProblemState],
    nbBacktracks: Int, maxBacktracks: Int, nbAssignments: Int): (SolverResult, List[Branch], List[ProblemState], Int, Int) = {
    //if (Thread.interrupted()) throw new InterruptedException()

    val filtering = currentState andThen {
      s => filter.reduceAfter(modifiedVariable, s)
    }

    filtering match {

      case Contradiction =>
        if (stack == Nil) {
          (UNSAT, Nil, Nil, nbBacktracks, nbAssignments)
        } else if (maxBacktracks >= 0 && nbBacktracks >= maxBacktracks) {
          (RESTART, stack, stateStack, nbBacktracks, nbAssignments)
        } else {
          val lastBranch = stack.head

          logger.info(s"${stack.length - 1}: ${lastBranch.b2Desc}")

          mac(lastBranch.changed, stack.tail, lastBranch.b2, stateStack.tail, nbBacktracks + 1, maxBacktracks, nbAssignments)
        }

      case filteredState: ProblemState =>
        heuristic.branch(filteredState) match {
          case None =>
            require(problem.variables.forall(v => filteredState.dom(v).size == 1),
              s"Unassigned variables in:\n${problem.toString(filteredState)}")

            assert {
              for (c <- problem.constraints.find(c => !c.controlAssignment(filteredState))) {
                throw new AssertionError(s"solution does not satisfy ${c.toString(filteredState)}")
              }
              true
            }

            (SAT(extractSolution(filteredState)), stack, filteredState :: stateStack, nbBacktracks, nbAssignments)
          case Some(branching) =>

            logger.info(s"${stack.length}: ${branching.b1Desc} ($nbBacktracks / $maxBacktracks)");

            // val assignedState = filteredState.assign(pair)

            mac(branching.changed, branching :: stack, branching.b1, filteredState :: stateStack, nbBacktracks, maxBacktracks, nbAssignments + 1)

        }
    }

  }

  def reset() {
    //decisions = Nil
    restart = true
  }

  val searchMeasurer = org.scalameter.`package`
    .config(
      Key.exec.benchRuns -> params.getOrElse("mac.benchRuns", 1),
      Key.verbose -> logger.underlying.isInfoEnabled())

  var nbBacktracks = 0

  @tailrec
  private def nextSolution(
    modifiedVar: Seq[Variable],
    stack: List[Branch],
    currentState: Outcome,
    stateStack: List[ProblemState]): (SolverResult, List[Branch], List[ProblemState]) = {

    logger.info("MAC with " + maxBacktracks + " bt")

    val (macResult, macTime) =
      StatisticsManager.measure(
        mac(modifiedVar, stack, currentState, stateStack, nbBacktracks, maxBacktracks, nbAssignments),
        searchMeasurer.withWarmer(
          if (nbAssignments <= params.getOrElse("mac.warm", 0)) {
            new Warmer.Default
          } else {
            Warmer.Zero
          }))

    searchCpu += macTime

    val (sol, newStack, newStateStack, newBack, newAss) = macResult.get

    logger.info(s"Took $macTime (${(newAss - nbAssignments) * 1000 / macTime.value}  aps)");

    nbAssignments = newAss
    nbBacktracks = newBack

    if (sol == RESTART) {

      //val modified = ngl.noGoods(newStack)

      //      if (!filter.reduceAfter(modified)) {
      //        (UNSAT, Nil)
      //      } else {
      maxBacktracks = (maxBacktracks * btGrowth).toInt;
      nbBacktracks = 0
      nextSolution(Seq.empty, Nil, newStateStack.last, Nil)
      //      }

    } else { (sol, newStack, newStateStack) }
  }

  var currentStack: List[Branch] = Nil
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
          val (sol, stack, stateStack) = nextSolution(Seq.empty, Nil, state, Nil)
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
        nextSolution(Seq.empty, currentStack, Contradiction, currentStateStack.tail)
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
  var searchCpu: Quantity[Double] = Quantity(0.0, "ms")

  @Statistic
  var usedMem = 0L

  override def toString =
    "maintain generalized arc consistency - iterative";

}
