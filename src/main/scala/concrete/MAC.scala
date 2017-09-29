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

package concrete

;

import java.util.concurrent.TimeoutException

import com.typesafe.scalalogging.LazyLogging
import concrete.filter.{ACC, Filter}
import concrete.heuristic.restart.{Geometric, NoRestarts, RestartStrategy}
import concrete.heuristic.value.Lexico
import concrete.heuristic.{Branch, Heuristic, NewSolutionEvent}
import concrete.util.SparseSeq
import cspom.{Statistic, StatisticsManager}
import org.scalameter.{Key, MeasureBuilder, Quantity}

import scala.annotation.{elidable, tailrec}

object MAC {
  def apply(prob: Problem, params: ParameterManager): MAC = {
    new MAC(prob, params, Heuristic.default(params, prob.variables))
  }
}

final class MAC(prob: Problem, params: ParameterManager, val heuristic: Heuristic) extends Solver(prob, params) with LazyLogging {


  private lazy val finishAssignments = new Lexico(params)
  val filterClass: Class[_ <: Filter] = params.getOrElse("mac.filter", classOf[ACC])
  val measureMem: Boolean = params.getOrElse("mac.measureMem", false)
  val filter: Filter = filterClass.getConstructor(classOf[Problem], classOf[ParameterManager]).newInstance(problem, params);
  statistics.register("filter", filter)
  filter.contradictionListeners +:= heuristic
  val rsClass: Class[_ <: RestartStrategy] = params.classInPackage("mac.restart", "concrete.heuristic.restart", classOf[Geometric])
  val searchMeasurer: MeasureBuilder[Unit, Double] = org.scalameter.`package`
    .config(
      Key.exec.benchRuns -> params.getOrElse("mac.benchRuns", 1),
      Key.verbose -> false)
  @Statistic
  var nbAssignments = 1
  var restartStrategy: RestartStrategy = if (heuristic.shouldRestart) {
    rsClass.getConstructor(classOf[ParameterManager], classOf[Problem]).newInstance(params, problem)
  } else {
    new NoRestarts(params, problem)
  }

  @Statistic
  var nbRuns = 0


  var currentStack: List[Branch] = Nil
  var currentStateStack: List[ProblemState] =
    List(heuristic.compute(this, init(problem.initState.toState)))
  @Statistic
  var searchCpu: Quantity[Double] = Quantity(0.0, "ms")
  @Statistic
  var usedMem = 0L
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

        case _: Contradiction =>
          if (stack.isEmpty) {
            (UNSAT, Nil, Nil, nbAssignments)
          } else if (maxBacktracks == 0) {
            (RESTART, stack, stateStack, nbAssignments)
          } else {
            val lastBranch = stack.head

            logger.info(s"${stack.length - 1}: ${lastBranch.b2Desc}")

            mac(lastBranch.c2, stack.tail, lastBranch.b2, stateStack.tail, maxBacktracks - 1, nbAssignments)
          }

        case fs: ProblemState =>
          val futureVariables = fs.getData[Seq[Variable]](this)

          val candidates: Seq[Variable] = futureVariables.filter(v => !fs.dom(v).isAssigned)
          val filteredState = fs.updateData(this, candidates)

          heuristic.branch(filteredState, candidates)
            .orElse {
              // Find unassigned variables

              problem.variables.find(v => !filteredState.dom(v).isAssigned)
                .map { v =>
                  logger.info(s"Assigning unassigned variable $v")
                  finishAssignments.branch(v, filteredState.dom(v), filteredState)
                }
            } match {
            case None =>
              require(problem.variables.forall(v => filteredState.dom(v).isAssigned),
                s"Unassigned variables in:\n${problem.toString(filteredState)}")

              controlSolution(filteredState)

              (SAT(extractSolution(filteredState)), stack, filteredState :: stateStack, nbAssignments)
            case Some(branching) =>

              logger.info(s"${stack.length}: ${branching.b1Desc} ($maxBacktracks bt left)");

              mac(branching.c1, branching :: stack, branching.b1, filteredState :: stateStack, maxBacktracks, nbAssignments + 1)
          }
      }
    }

  }

  def reset() {

    restart = true
    restartStrategy.reset()
  }

  def oneRun(modified: Seq[(Variable, Event)],
             stack: List[Branch],
             currentState: Outcome,
             stateStack: List[ProblemState]): (SolverResult, List[Branch], List[ProblemState]) = {
    nbRuns += 1
    val maxBacktracks = restartStrategy.nextRun()
    logger.info(s"MAC with $maxBacktracks bt")

    running = true

    val (macResult, macTime) =
      StatisticsManager.measure(
        mac(modified, stack, currentState, stateStack, maxBacktracks, nbAssignments),
        searchMeasurer)

    searchCpu += macTime
    running = false

    val (sol, newStack, newStateStack, newAss) = macResult.get

    logger.info(s"Took $macTime (${(newAss - nbAssignments) * 1000 / macTime.value}  aps)")

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

  def nextSolution(): SolverResult = try {

    logger.info(heuristic.toString)

    val sol = if (restart) {
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
        nextSolution(Seq.empty, currentStack, Contradiction(Seq()), extendedStack)
      currentStack = stack
      currentStateStack = stateStack
      sol
    }
    for (s <- sol.getInt) heuristic.event(NewSolutionEvent(s))
    sol
  } finally {
    if (measureMem || usedMem == 0L) {
      val runtime = Runtime.getRuntime
      System.gc()
      System.gc()
      System.gc()
      System.gc()
      System.gc()
      usedMem = math.max(usedMem, runtime.totalMemory - runtime.freeMemory)
    }
  }

  override def toString =
    "maintain generalized arc consistency - iterative";

  def init(ps: ProblemState): ProblemState = {
    ps.updateData(this, SparseSeq(heuristic.decisionVariables: _*))
  }


  @elidable(elidable.ASSERTION)
  private def controlSolution(ps: ProblemState): Unit = {
    for (c <- problem.constraints.find(c => !c.controlAssignment(ps))) {
      throw new AssertionError(s"solution does not satisfy ${c.toString(ps)}")
    }
  }

}
