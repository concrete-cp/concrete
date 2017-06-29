package concrete
package heuristic
package variable

import java.util.EventObject

import com.typesafe.scalalogging.LazyLogging
import concrete.util.SparseSeq

import scala.annotation.tailrec
import scala.util.Random

abstract class VariableHeuristic(
                                  params: ParameterManager,
                                  val decisionVariables: Array[Variable]) extends LazyLogging {

  val randomBreak = params.getOrElse("heuristic.variable.randomBreak", true)
  val randomDiv = params.getOrElse("heuristic.variable.randomDiv", 0.2)
  val candidates = new Array[Variable](decisionVariables.length)
  protected val rand: Random = {
    val seed = params.getOrElse("randomseed", 0L) + params.getOrElse("iteration", 0)
    new Random(seed)
  }

  def compute(state: ProblemState): ProblemState =
    state.updateData(this, SparseSeq(decisionVariables: _*))

  final def select(state: ProblemState): Option[(Variable, ProblemState)] = {
    val oc = state.getData[SparseSeq[Variable]](this)

    val candidates = oc.filter(v => !state.dom(v).isAssigned)

    // println(s"${oc.size} ${candidates.size}")

    if (candidates.isEmpty) {
      None
    } else if (rand.nextDouble() < randomDiv) {
      Some((
        candidates(rand.nextInt(candidates.size)),
        state.updateData(this, candidates)))
    } else {
      Some((select(candidates, state), state.updateData(this, candidates)))
    }

  }

  def select(i: Traversable[Variable], state: ProblemState): Variable = {
    val selected = if (randomBreak) {
      //selectRandTB(i + 1, v, score(v, state.dom(v), state), 2, state, rand)
      selectRandTB2(i, state, rand)
    } else {
      selectFirst2(i, state)
    }
    logger.debug(s"$selected : ${score(selected, state.dom(selected), state)}")
    selected
  }

  def compare(v1: Variable, d1: Domain, v2: Variable, d2: Domain, state: ProblemState): Int =
    java.lang.Double.compare(score(v1, d1, state), score(v2, d2, state))

  def score(variable: Variable, domain: Domain, state: ProblemState): Double

  def event(e: EventObject): Unit = ()

  def shouldRestart: Boolean = randomBreak || randomDiv > 0

  private def selectRandTB2(i: Traversable[Variable], state: ProblemState, rand: Random): Variable = {
    var lastCand = 0
    var bs = Double.NegativeInfinity

    for (current <- i) {
      val dom = state.dom(current)
      val s = score(current, dom, state)
      if (s > bs) {
        lastCand = 1
        candidates(0) = current
        bs = s
      } else if (s == bs) {
        candidates(lastCand) = current
        lastCand += 1
      }
    }
    candidates(rand.nextInt(lastCand))
  }

  private def selectFirst2(i: Traversable[Variable], state: ProblemState): Variable = {
    var b: Variable = null
    var bs = Double.NegativeInfinity

    for (current <- i) {
      val dom = state.dom(current)
      val s = score(current, dom, state)
      if (s > bs) {
        b = current
        bs = s
      }
    }
    b
  }

  @tailrec
  private def selectFirst(i: List[Variable], best: Variable, bestScore: Double, state: ProblemState): Variable = {
    i match {
      case Nil => best
      case current :: tail =>
        val dom = state.dom(current)
        if (dom.isAssigned) {
          selectFirst(i, best, bestScore, state)
        } else {
          val s = score(current, dom, state)
          if (s > bestScore) {
            selectFirst(tail, current, s, state)
          } else {
            selectFirst(tail, best, bestScore, state)
          }
        }
    }
  }
}
