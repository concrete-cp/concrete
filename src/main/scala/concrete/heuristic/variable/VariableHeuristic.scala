package concrete
package heuristic
package variable

import scala.annotation.tailrec
import scala.util.Random

import com.typesafe.scalalogging.LazyLogging
import java.util.EventObject

abstract class VariableHeuristic(params: ParameterManager, val decisionVariables: Array[Variable]) extends LazyLogging {

  protected val rand: Random = {
    val seed = params.getOrElse("randomseed", 0L) + params.getOrElse("iteration", 0)
    new Random(seed)
  }

  val randomBreak = params.getOrElse("heuristic.variable.randomBreak", true)

  val randomDiv = params.getOrElse("heuristic.variable.randomDiv", 0.2)

  final def select(state: ProblemState): Option[Variable] = {
    var i = 0
    while (i < decisionVariables.length && state.dom(decisionVariables(i)).isAssigned) {
      i += 1
    }
    if (i >= decisionVariables.length) {
      None
    } else if (rand.nextDouble() < randomDiv) {
      Some(selectRand(i, decisionVariables(i), rand.nextDouble(), state, rand))
    } else {
      Some(select(i, state))
    }
  }
  //def problem: Problem

  @tailrec
  private def selectRand(i: Int, best: Variable, bestScore: Double, state: ProblemState, rand: Random): Variable = {
    if (i >= decisionVariables.length) {
      logger.debug(s"$best: $bestScore")
      best
    } else {
      val current = decisionVariables(i)
      val dom = state.dom(current)
      if (dom.isAssigned) {
        selectRand(i + 1, best, bestScore, state, rand)
      } else {
        val s = rand.nextDouble()

        if (s < bestScore) {
          selectRand(i + 1, current, s, state, rand)
        } else {
          selectRand(i + 1, best, bestScore, state, rand)
        }

      }
    }
  }

  @tailrec
  private def selectRandTB(i: Int, best: Variable, bestScore: Double, ties: Int, state: ProblemState, rand: Random): Variable = {
    if (i >= decisionVariables.length) {
      logger.debug(s"$best: $bestScore")
      best
    } else {
      val current = decisionVariables(i)
      val dom = state.dom(current)
      if (dom.isAssigned) {
        selectRandTB(i + 1, best, bestScore, ties, state, rand)
      } else {
        val s = score(current, dom, state)

        if (s > bestScore) {
          selectRandTB(i + 1, current, s, 2, state, rand)
        } else if (s < bestScore) {
          selectRandTB(i + 1, best, bestScore, ties, state, rand)
        } else if (rand.nextDouble() * ties < 1) {
          selectRandTB(i + 1, current, s, ties + 1, state, rand)
        } else {
          selectRandTB(i + 1, best, bestScore, ties + 1, state, rand)
        }

      }
    }
  }

  @tailrec
  private def selectFirst(i: Int, best: Variable, bestScore: Double, state: ProblemState): Variable = {
    if (i >= decisionVariables.length) {
      best
    } else {
      val current = decisionVariables(i)
      val dom = state.dom(current)
      if (dom.isAssigned) {
        selectFirst(i + 1, best, bestScore, state)
      } else {
        val s = score(current, dom, state)
        if (s > bestScore) {
          selectFirst(i + 1, current, s, state)
        } else {
          selectFirst(i + 1, best, bestScore, state)
        }
      }
    }
  }

  def select(i: Int, state: ProblemState): Variable = {
    val v = decisionVariables(i)
    if (randomBreak) {
      selectRandTB(i + 1, v, score(v, state.dom(v), state), 2, state, rand)
    } else {
      selectFirst(i + 1, v, score(v, state.dom(v), state), state)
    }
  }

  def compare(v1: Variable, d1: Domain, v2: Variable, d2: Domain, state: ProblemState): Int =
    java.lang.Double.compare(score(v1, d1, state), score(v2, d2, state))

  def score(variable: Variable, domain: Domain, state: ProblemState): Double

  def event(e: EventObject): Unit = ()

  def shouldRestart: Boolean = randomBreak || randomDiv > 0
}
