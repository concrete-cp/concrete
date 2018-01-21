package concrete.heuristic.variable


import java.util.EventObject

import com.typesafe.scalalogging.LazyLogging
import concrete.heuristic.{Assign, BadDecision}
import concrete.{MAC, Outcome, ProblemState, Variable}

import scala.collection.mutable

class LastConflict(val pool: Seq[Variable]) extends VariableHeuristic with LazyLogging {
  val K = 2
  val testingSet = new mutable.HashSet[Variable]()
  var candidate: Option[Variable] = None

  override def select(state: ProblemState, candidates: Seq[Variable]): Seq[Variable] = {
    logger.info(s"Testing set: $testingSet, candidate: $candidate")
    val ts = testingSet.iterator.filter(x => !state.dom(x).isAssigned).toSeq

    if (ts.isEmpty) {
      val c = candidate.filter(x => !state.dom(x).isAssigned)
      candidate = None
      if (c.isEmpty) {
        testingSet.clear()
        candidates
      } else {
        testingSet ++= c
        c.toSeq
      }
    } else {
      ts
    }
  }

  override def shouldRestart: Boolean = false

  override def compute(s: MAC, ps: ProblemState): ProblemState = ps

  override def event[S <: Outcome](e: EventObject, state: S): S = {
    e match {
      case BadDecision(Assign(x, _)) if candidate.isEmpty && testingSet.size <= K && !testingSet(x) =>
        logger.debug(s"new candidate $x")
        candidate = Some(x)
      case _ =>
    }
    state
  }
}
