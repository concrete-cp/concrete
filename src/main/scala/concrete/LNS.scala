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

import scala.util.Random

import com.typesafe.scalalogging.LazyLogging

import concrete.generator.cspompatterns.XCSPPatterns
import concrete.heuristic.value.Lexico
import cspom.CSPOM
import cspom.Statistic
import cspom.StatisticsManager
import cspom.compiler.CSPOMCompiler
import org.scalameter.Quantity

object LNS extends App {
  def apply(prob: Problem, params: ParameterManager): LNS = {
    val mac = MAC(prob, params)
    new LNS(prob, params, mac)
  }

  val prob = CSPOM.load("/home/vion/xcsp3series/Scheduling/Scheduling-os-taillard/Taillard-os-05-05-0.xml.lzma").get
  val params = new ParameterManager
  params("solver") = classOf[LNS]
  params("heuristic.value") = classOf[Lexico]

  CSPOMCompiler.compile(prob, XCSPPatterns()).get

  val solv = Solver(prob, params).get

  val stats = new StatisticsManager()
  stats.register("solver", solv.solver)

  for (s <- solv) {
    println(s("cspom-objective"))
    println(stats)
  }
}

final class LNS(prob: Problem, params: ParameterManager, val mac: MAC) extends Solver(prob, params) with LazyLogging {

  val nbFixed: Double = .4

  val nbCalls = 50

  val limit = 500

  var init = prob.initState.toState

  @Statistic
  var assignCpu: Quantity[Double] = Quantity(0.0, "ms")

  @Statistic
  var nbIterations = 0
  @Statistic
  var nbFragments = 0

  statistics.register("mac", mac)

  var best: Option[Map[Variable, Int]] = None // prob.variables.map(init.dom(_).head)

  lazy val objIndex = prob.variables.indexOf(optimises.get)

  def reset() {
    mac.reset()
  }

  val rand = new Random(0)

  private def fragment(fragSize: Int): Seq[Int] = {
    require(objIndex >= 0)
    // Don't assign optimisation variable
    (concrete.util.Math.randSet(fragSize, prob.nbVariables, rand) - objIndex).toSeq
  }

  @annotation.tailrec
  private def selectRand(pool: Iterator[Variable], best: Variable, bestScore: Double, ties: Int, score: Variable => Double, rand: Random): Variable = {
    if (pool.hasNext) {
      val current = pool.next

      val s = score(current)

      if (s > bestScore) {
        selectRand(pool, current, s, 2, score, rand)
      } else if (s < bestScore) {
        selectRand(pool, best, bestScore, ties, score, rand)
      } else if (rand.nextDouble() * ties < 1) {
        selectRand(pool, current, s, ties + 1, score, rand)
      } else {
        selectRand(pool, best, bestScore, ties + 1, score, rand)
      }

    } else {
      best
    }
  }

  lazy val initPool = problem.variables.iterator.filter(!init.dom(_).isAssigned).toSet - optimises.get

  private def assignFragment(init: ProblemState, best: Map[Variable, Int], fragSize: Int): ProblemState = {
    var pool = initPool
    var size = 0
    var ps = init

    def score(variable: Variable) = {
      if (ps.dom(variable).isAssigned) {
        pool -= variable
        -2
      } else {
        -ps.card(variable) / init.card(variable)
      }
    }

    //println("Selecting fragment")

    while (size < fragSize) {

      val it = pool.iterator
      if (!it.hasNext) return ps
      val first = it.next()
      val selected = selectRand(it, first, score(first), 2, score, rand)

      pool -= selected

      //println(selected + " " + init.card(selected) + " " + ps.card(selected))
      ps = ps.tryAssign(selected, best(selected))
        .andThen(mac.filter.reduceAfter(Seq((selected, Assignment)), _))
        .orElse(ps)

      size += 1
    }
    ps
  }

  private def randomAssign(ps: ProblemState, best: Map[Variable, Int], fragSize: Int) = {
    //println("assigning")
    //var size = 0
    // Cancel assignation if inconsistent
    fragment(fragSize).foldLeft(ps) { (ps, pos) =>
      val variable = prob.variables(pos)
      //size += 1
      ps.tryAssign(variable, best(variable))
        .andThen(mac.filter.reduceAfter(Seq((variable, Assignment)), _))
        .orElse {
          //size -= 1; 
          ps
        }
    }

  }

  @annotation.tailrec
  private def nextSolution(currentFragment: ProblemState, nbCall: Int): SolverResult = {
    if (nbCall == 0) {

      //print(ps.dom(optimises.get))
      nbFragments += 1
      val (frag, time) = StatisticsManager.measure {
        best.foldLeft(init) { (ps, best) =>
          assignFragment(ps, best, (prob.nbVariables * nbFixed).toInt)
        }
      }

      assignCpu += time

      nextSolution(frag.get, nbCalls)

    } else {

      //print(currentFragment.dom(optimises.get))

      nbIterations += 1
      // TODO mac.maxBacktracks = limit
      val (result, _, stack) = mac.oneRun(Seq(), List(), currentFragment, List())

      //println(result)

      //for (root <- stack.lastOption) init = root

      result match {
        case s: SAT =>
          best = s.getInt
          s
        case UNSAT | RESTART => nextSolution(currentFragment, nbCall - 1)
        case e => e
      }

    }
  }

  def nextSolution(): SolverResult = {
    init.padConstraints(problem.constraints, problem.maxCId)
      .andThen(mac.filter.reduceAll) match {
        case ps: ProblemState =>
          init = ps
          nextSolution(null, 0)
        case _: Contradiction => UNSAT
      }

  }

  override def toString = "lns"

}


