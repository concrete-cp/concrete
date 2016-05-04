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

import java.util.concurrent.BlockingQueue
import java.util.concurrent.LinkedBlockingDeque

import com.typesafe.scalalogging.LazyLogging

import concrete.filter.ACC
import concrete.heuristic.Heuristic
import concrete.heuristic.CrossHeuristic

object EPS {
  def apply(prob: Problem, params: ParameterManager): EPS = {
    val heuristic: Heuristic = params.get[Any]("mac.heuristic").getOrElse(classOf[CrossHeuristic]) match {
      case heuristicClass: Class[_] =>
        heuristicClass
          .getMethod("apply", classOf[ParameterManager], classOf[Array[Variable]])
          .invoke(null, params, prob.variables.toArray)
          .asInstanceOf[Heuristic]
      case heuristic: Heuristic => heuristic
    }

    new EPS(prob, params, heuristic)
  }
}

final class EPS(prob: Problem, params: ParameterManager, val heuristic: Heuristic) extends Solver(prob, params) with LazyLogging {

  def reset() {
    //decisions = Nil
  }

  val initState = prob.initState

  val queue: BlockingQueue[(ProblemState, Seq[Variable])] = new LinkedBlockingDeque

  val filter = new ACC(prob, params)

  def nextSolution(): SolverResult = {
    val init = prob.initState
      .andThen { s => filter.reduceAll(s) }
      .andThen { s => queue.add((s, Seq())); s }

    init match {
      case Contradiction => UNSAT
      case _ =>
        do {
          produceConsume() match {
            case s: SAT                     => return s
            case UNSAT if queue.isEmpty     => return UNSAT
            case u @ UNKNOWNResult(Some(_)) => return u
            case _                          =>
          }
        } while (true)

        throw new IllegalStateException
    }

  }

  def produceConsume(): SolverResult = {
    val (state, vars) = queue.take()
    filter.reduceAfter(vars, state) match {
      case Contradiction => UNSAT
      case ps: ProblemState =>
        heuristic.branch(ps) match {
          case None =>
            SAT(extractSolution(ps))
          case Some(branch) =>
            queue.add((branch.b1, branch.changed))
            queue.add((branch.b2, branch.changed))
            UNKNOWNResult(None)
        }
    }
  }

  override def toString =
    "embarassingly parallel search";

}
