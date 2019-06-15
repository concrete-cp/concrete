package concrete.heuristic.variable

import java.util.EventObject

import bitvectors.BitVector
import com.typesafe.scalalogging.LazyLogging
import concrete._

import scala.collection.mutable.ArrayBuffer

abstract class ScoredVariableHeuristic extends VariableHeuristic with LazyLogging {
  private val poolSet = BitVector(pool.map(_.id))

  def select(state: ProblemState, i: Seq[Variable]): Seq[Variable] = {
    var bs = Double.NegativeInfinity
    val candidates = new ArrayBuffer[Variable]()

    for (current <- i if poolSet(current.id)) {
      val s = score(current, state.dom(current), state)
      if (s >= bs) {
        if (s > bs) {
          candidates.clear()
          bs = s
        }
        candidates += current
      }
    }

    logger.info(s"Score $bs for candidates: $candidates")
    candidates.toSeq
  }

  def compare(v1: Variable, d1: Domain, v2: Variable, d2: Domain, state: ProblemState): Int =
    java.lang.Double.compare(score(v1, d1, state), score(v2, d2, state))

  def score(variable: Variable, domain: Domain, state: ProblemState): Double

  def event[S <: Outcome](e: EventObject, ps: S): S = ps
}
