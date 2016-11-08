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

import concrete.filter.ACC
import concrete.filter.Filter
import concrete.heuristic.CrossHeuristic
import concrete.heuristic.Heuristic
import com.typesafe.scalalogging.LazyLogging
import cspom.Statistic
import cspom.StatisticsManager
import scala.annotation.tailrec
import concrete.heuristic.Branch
import org.scalameter.Quantity
import org.scalameter.Key
import org.scalameter.Warmer
import java.util.concurrent.TimeoutException
import scala.annotation.elidable
import concrete.heuristic.restart.Geometric
import concrete.heuristic.restart.NoRestarts
import concrete.heuristic.restart.RestartStrategy

object MAC {
  def apply(prob: Problem, params: ParameterManager): MAC = {

    new MAC(prob, params, Heuristic.default(params, prob.variables.toArray))
  }
}

final class MAC(prob: Problem, params: ParameterManager, val heuristic: Heuristic) extends Solver(prob, params) with LazyLogging {

  val filterClass: Class[_ <: Filter] = params.getOrElse("mac.filter", classOf[ACC])

  val measureMem: Boolean = params.getOrElse("mac.measureMem", false)

  @Statistic
  var nbAssignments = 1;

  val filter: Filter = filterClass.getConstructor(classOf[Problem], classOf[ParameterManager]).newInstance(problem, params);
  statistics.register("filter", filter);

  val rsClass: Class[_ <: RestartStrategy] = params.classInPackage("mac.restart", "concrete.heuristic.restart", classOf[Geometric])

  val resetOnRestart: Boolean = params.contains("mac.resetOnRestart")

  var restartStrategy = if (heuristic.shouldRestart) {
    rsClass.getConstructor(classOf[ParameterManager], classOf[Problem]).newInstance(params, problem)
  } else { new NoRestarts(params, problem) }

  private var restart = true
  private var firstRun = true

  @tailrec
  def mac(
    modified: Seq[(Variable, Event)], stack: List[Branch],
    currentState: Outcome, stateStack: List[ProblemState],
    maxBacktracks: Int, nbAssignments: Int): (SolverResult, List[Branch], List[ProblemState], Int) = {
    if (Thread.interrupted()) {
      (UNKNOWNResult(new TimeoutException()), stack, stateStack, nbAssignments)
    } else {

      val filtering = currentState
        .andThen(s => filter.reduceAfter(modified, s))

      filtering match {

        case Contradiction =>
          if (stack.isEmpty) {
            (UNSAT, Nil, Nil, nbAssignments)
          } else if (maxBacktracks == 0) {
            (RESTART, stack, stateStack, nbAssignments)
          } else {
            val lastBranch = stack.head

            logger.info(s"${stack.length - 1}: ${lastBranch.b2Desc}")

            mac(lastBranch.c2, stack.tail, lastBranch.b2, stateStack.tail, maxBacktracks - 1, nbAssignments)
          }

        case filteredState: ProblemState =>
          heuristic.branch(filteredState) match {
            case None =>
              require(problem.variables.forall(v => filteredState.assigned(v)),
                s"Unassigned variables in:\n${problem.toString(filteredState)}")

              controlSolution(filteredState)

              (SAT(extractSolution(filteredState)), stack, filteredState :: stateStack, nbAssignments)
            case Some(branching) =>

              if (nbAssignments % 1000 == 0) logger.info(s"$nbAssignments assignments")

              logger.info(s"${stack.length}: ${branching.b1Desc} ($maxBacktracks bt left)");

              mac(branching.c1, branching :: stack, branching.b1, filteredState :: stateStack, maxBacktracks, nbAssignments + 1)
          }
      }
    }

  }

  @elidable(elidable.ASSERTION)
  private def controlSolution(ps: ProblemState): Unit = {
    for (c <- problem.constraints.find(c => !c.controlAssignment(ps))) {
      throw new AssertionError(s"solution does not satisfy ${c.toString(ps)}")
    }
  }

  def reset() {
    restart = true
    if (resetOnRestart)
      restartStrategy.reset()
  }

  val searchMeasurer = org.scalameter.`package`
    .config(
      Key.exec.benchRuns -> params.getOrElse("mac.benchRuns", 1),
      Key.verbose -> false)

  def oneRun(modified: Seq[(Variable, Event)],
    stack: List[Branch],
    currentState: Outcome,
    stateStack: List[ProblemState]): (SolverResult, List[Branch], List[ProblemState]) = {
    val maxBacktracks = restartStrategy.nextRun()
    logger.info(s"MAC with $maxBacktracks bt")

    running = true

    val (macResult, macTime) =
      StatisticsManager.measure(
        mac(modified, stack, currentState, stateStack, maxBacktracks, nbAssignments),
        searchMeasurer.withWarmer(
          if (nbAssignments <= params.getOrElse("mac.warm", 0)) {
            new Warmer.Default
          } else {
            Warmer.Zero
          }))

    searchCpu += macTime
    running = false

    val (sol, newStack, newStateStack, newAss) = macResult.get

    logger.info(s"Took $macTime (${(newAss - nbAssignments) * 1000 / macTime.value}  aps)");

    nbAssignments = newAss
    (sol, newStack, newStateStack)
  }

  @tailrec
  def nextSolution(
    modified: Seq[(Variable, Event)],
    stack: List[Branch],
    currentState: Outcome,
    stateStack: List[ProblemState]): (SolverResult, List[Branch], List[ProblemState]) = {

    val (sol, newStack, newStateStack) = oneRun(modified, stack, currentState, stateStack)

    if (sol == RESTART) {
      nextSolution(Seq.empty, Nil, newStateStack.last, Nil)
    } else {
      (sol, newStack, newStateStack)
    }
  }

  var currentStack: List[Branch] = Nil
  var currentStateStack: List[ProblemState] = List(problem.initState.toState)

  def nextSolution(): SolverResult = try {

    logger.info(heuristic.toString)

    if (restart) {
      logger.info("RESTART")
      restart = false

      currentStateStack.last
        .padConstraints(problem.constraints, problem.maxCId)
        .andThen { ps =>
          if (firstRun) {
            firstRun = false
            preprocess(filter, ps)
          } else filter.reduceAll(ps)
        }
        .map { state =>
          val (sol, stack, stateStack) = nextSolution(Seq.empty, Nil, state, Nil)
          currentStack = stack
          currentStateStack = stateStack
          logger.info(s"Search ended with $sol")
          sol
        }
        .getOrElse(UNSAT)

    } else if (currentStack.isEmpty) {
      UNSAT
    } else {
      restartStrategy = new NoRestarts(params, problem)

      // backtrack once (.tail) and 
      // extends state stack for new constraints
      val extendedStack = currentStateStack.tail.map(s =>
        s.padConstraints(problem.constraints, problem.maxCId).toState)

      /* Contradiction will trigger a backtrack */

      val (sol, stack, stateStack) =
        nextSolution(Seq.empty, currentStack, Contradiction, extendedStack)
      currentStack = stack
      currentStateStack = stateStack
      sol
    }

  } finally {
    if (measureMem || usedMem == 0L) {
      val runtime = Runtime.getRuntime()
      System.gc()
      System.gc()
      System.gc()
      System.gc()
      System.gc()
      usedMem = math.max(usedMem, runtime.totalMemory - runtime.freeMemory)
    }
  }

  @Statistic
  var searchCpu: Quantity[Double] = Quantity(0.0, "ms")

  @Statistic
  var usedMem = 0L

  override def toString =
    "maintain generalized arc consistency - iterative";

}
