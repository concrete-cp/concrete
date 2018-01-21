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

import java.util.concurrent.TimeoutException

import com.typesafe.scalalogging.LazyLogging
import concrete.constraint.Constraint
import concrete.constraint.linear.{GtC, LtC}
import concrete.constraint.semantic.NoGoods
import concrete.filter.{ACC, Filter}
import concrete.heuristic._
import concrete.heuristic.restart.{Geometric, NoRestarts, RestartStrategy}
import concrete.heuristic.value.Lexico
import concrete.util.SparseSeq
import cspom.{Statistic, StatisticsManager}
import org.scalameter.{Key, MeasureBuilder, Quantity}

import scala.annotation.{elidable, tailrec}
import scala.util.{Random, Try}

object MAC {
  def apply(prob: Problem, params: ParameterManager): Try[MAC] = {

    val rand = {
      val seed = params.getOrElse("randomseed", 0L) + params.getOrElse("iteration", 0)
      new Random(seed)
    }

    for (h <- Heuristic.default(params, prob.variables, rand)) yield {
      new MAC(prob, params, h)
    }
  }
}

final class MAC(prob: Problem, params: ParameterManager, val heuristic: Heuristic) extends Solver(prob, params) with LazyLogging {


  private lazy val finishAssignments = new Lexico()
  val filterClass: Class[_ <: Filter] = params.getOrElse("mac.filter", classOf[ACC])
  val measureMem: Boolean = params.getOrElse("mac.measureMem", false)
  val filter: Filter = filterClass.getConstructor(classOf[Problem], classOf[ParameterManager]).newInstance(problem, params)
  statistics.register("filter", filter)
  filter.contradictionListeners +:= heuristic
  val rsClass: Class[_ <: RestartStrategy] = params.classInPackage("mac.restart", "concrete.heuristic.restart", classOf[Geometric])
  val searchMeasurer: MeasureBuilder[Unit, Double] = org.scalameter.`package`
    .config(
      Key.exec.benchRuns -> params.getOrElse("mac.benchRuns", 1),
      Key.verbose -> false)


  private val superNG = params.contains("superng")

  private val nogoods = if (problem.variables.nonEmpty && (params.contains("nogoods") || superNG)) Some(new NoGoods(problem.variables)) else None

  @Statistic
  var nbAssignments = 1
  var restartStrategy: RestartStrategy = if (heuristic.shouldRestart) {
    rsClass.getConstructor(classOf[ParameterManager], classOf[Problem]).newInstance(params, problem)
  } else {
    new NoRestarts(params, problem)
  }
  @Statistic
  var nbRuns = 0
  var currentStack: Stack = Stack(problem.initState)
  var currentBTLeft: Option[Int] = None

  @Statistic
  var searchCpu: Quantity[Double] = Quantity(0.0, "ms")
  @Statistic
  var usedMem = 0L
  private var restart = true
  private var firstRun = true

  override def addConstraint[A <: Constraint](c: A): A = {
    super.addConstraint(filter.addConstraint(c))
  }

  nogoods.foreach(addConstraint)
  nogoods.foreach(statistics.register("nogoods", _))

  def learnNoGoods(history: List[Seq[Decision]], stack: List[Outcome]): Unit = {


    for (ngCons <- nogoods) {
      ngCons.clearInactive()
      var positive: List[Assign] = Nil

      if (superNG) {
        if (history.nonEmpty) {
          for ((h, Seq(s1: ProblemState, s2: ProblemState)) <- (history.reverse.tail, stack.reverse.sliding(2).toSeq).zipped) {
            var i = 0
            //println("new level")
            //println(h)
            val decision = h.last match {
              case a: Assign => a
            }
            positive ::= decision
            //println(decision)
            for (v <- problem.variables) {
              val d1 = s1.dom(v)
              val d2 = s2.dom(v)
              if (d1 ne d2) {
                if (ngCons.addNoGood(positive, v, d2).isDefined) {
                  i += 1
                } else {
                  return
                }
                //println(s"$v: $d1 -> $d2")
              }
            }
            logger.info(s"Learned $i nogoods of size ${positive.size + 1}")
          }
        }


      } else {
        val hist = history.flatten.reverse
        // logger.info(s"history: ${hist.mkString("\n")}")


        for (d <- hist) {
          d match {
            case a: Assign => positive ::= a
            case r: Remove => ngCons.addNoGood(positive, r)
            case _ =>
          }
        }
      }
    }

  }

  @tailrec
  def mac(
           modified: Seq[(Variable, Event)],
           stack: Stack,
           maxBacktracks: Option[Int], nbAssignments: Int): (SolverResult, Stack, Option[Int], Int) = {


    val filtering = stack.current.andThen(s => filter.reduceAfter(modified, optimConstraint, s))

    filtering match {

      case c: Contradiction =>
        if (stack.noRightStack) {
          (UNSAT, Stack(c), maxBacktracks, nbAssignments)
        } else {
          for (level <- stack.decisionHistory.headOption; decision <- level.lastOption) {
            heuristic.event(BadDecision(decision), c)
          }

          val (newStack, modified) = stack.backtrackAndApplyRightDecision

          logger.info(s"${newStack.size}: ${stack.rightStack.head.toString(stack.leftStack.head)}")

          logger.info(s"$maxBacktracks bt left")

          // rightStack state replaces head state
          mac(modified, newStack, maxBacktracks.map(_ - 1), nbAssignments)
        }

      case fs if Thread.interrupted() => (UNKNOWNResult(new TimeoutException()), stack.copy(current = fs), maxBacktracks, nbAssignments)

      case fs if maxBacktracks.exists(_ <= 0) =>
        (RESTART, stack.copy(current = fs), maxBacktracks, nbAssignments)

      case fs: ProblemState =>
        val futureVariables = fs.getData[Seq[Variable]](this)

        val (assigned, candidates) = futureVariables.partition(v => fs.dom(v).isAssigned)

        val filteredState =
          heuristic.event(AssignmentEvent(assigned: _*), fs).updateData(this, candidates)

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

            (SAT(extractSolution(filteredState)), stack.copy(current = filteredState), maxBacktracks, nbAssignments)
          case Some((b1, b2)) =>
            logger.info(s"${stack.size}: ${b1.toString(filteredState)}")

            val (newStack, modified) = stack.push(filteredState, b1, b2)
            mac(modified, newStack, maxBacktracks, nbAssignments + 1)
        }


    }
  }

  def reset() {

    restart = true
    restartStrategy.reset()
  }

  def oneRun(modified: Seq[(Variable, Event)],
             stack: Stack,
             maxBacktracks: Option[Int]
            ): (SolverResult, Stack, Option[Int]) = {
    nbRuns += 1

    logger.info(s"MAC with $maxBacktracks bt")

    running = true

    val (macResult, macTime) =
      StatisticsManager.measure(
        mac(modified, stack, maxBacktracks, nbAssignments),
        searchMeasurer)

    searchCpu += macTime
    running = false

    val (sol, newStack, btLeft, newAss) = macResult.get

    logger.info(s"Took $macTime (${(newAss - nbAssignments) * 1000 / macTime.value}  aps)")

    nbAssignments = newAss
    (sol, newStack, btLeft)
  }

  @tailrec
  def nextSolution(
                    modified: Seq[(Variable, Event)],
                    stack: Stack,
                    maxBacktracks: Option[Int]): (SolverResult, Stack, Option[Int]) = {

    val (result, newStack, btLeft) = oneRun(modified, stack, maxBacktracks)

    result match {
      case RESTART =>
        learnNoGoods(newStack.decisionHistory, newStack.leftStack)
        val maxBT = restartStrategy.nextRun()
        nextSolution(Seq.empty, newStack.last, maxBT)
      case SAT(sol) =>
        problem.goal match {
          case Maximize(v) =>
            sol(v) match {
              case i: Int =>
                val oc = obtainOptimConstraint(new GtC(v, i))
                oc.constant = i
                logger.info(s"new best value $i")
              case o => throw new AssertionError(s"$v has value $o which is not an int")
            }


          case Minimize(v) =>
            sol(v) match {
              case i: Int =>
                val oc = obtainOptimConstraint(new LtC(v, i))
                oc.constant = i
                logger.info(s"new best value $i")
              case o => throw new AssertionError(s"$v has value $o which is not an int")
            }

          case Satisfy =>
        }
        assert(problem.constraints.forall(_.positionInVariable.forall(_ >= 0)))

        // Change current state to Contradiction so that next run will begin by a backtrack
        (result, newStack.copy(current = Contradiction(Seq())), btLeft)
      case _ =>
        (result, newStack, btLeft)
    }
  }

  def init(ps: ProblemState): Outcome = {
    heuristic.compute(this,
      ps.updateData(this, SparseSeq(heuristic.decisionVariables: _*)))
  }

  def nextSolution(): SolverResult = try {

    logger.info(heuristic.toString)

    val sol = if (restart) {
      logger.info("RESTART")
      restart = false
      currentStack.last.padConstraints(problem).current
        .andThen { ps =>
          if (firstRun) {
            firstRun = false
            preprocess(filter, ps).andThen(init)
          } else {
            assert(ps eq filter.reduceAll(ps))
            ps
          }
        }
        .map { state =>
          val bt = restartStrategy.nextRun()
          val (sol, stack, btLeft) = nextSolution(Seq.empty, Stack(state), bt)
          currentBTLeft = btLeft
          currentStack = stack
          logger.info(s"Search ended with $sol")
          sol
        }
        .getOrElse(UNSAT)

    } else if (currentStack.noRightStack) {
      UNSAT
    } else {
      if (problem.goal == Satisfy) {
        // Stop restarts to enumerate solutions
        currentBTLeft = None
      }

      // extends state stack for new constraints
      val extendedStack = currentStack.padConstraints(problem)

      val (sol, stack, btLeft) = nextSolution(Seq.empty, extendedStack, currentBTLeft)
      currentBTLeft = btLeft
      currentStack = stack
      sol
    }
    for (s <- sol.getInt) heuristic.event(NewSolutionEvent(s), null)
    sol
  } finally {
    if (measureMem) {
      val runtime = Runtime.getRuntime
      System.gc()
      System.gc()
      System.gc()
      System.gc()
      System.gc()
      usedMem = math.max(usedMem, runtime.totalMemory - runtime.freeMemory)
    }
  }

  override def toString: String = "maintain generalized arc consistency - iterative"

  @elidable(elidable.ASSERTION)
  private def controlSolution(ps: ProblemState): Unit = {
    for (c <- problem.constraints.find(c => !c.controlAssignment(ps))) {
      throw new AssertionError(s"solution does not satisfy ${c.toString(ps)}")
    }
  }

}
