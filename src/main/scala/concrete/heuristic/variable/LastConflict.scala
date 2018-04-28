package concrete.heuristic.variable


import java.util.EventObject

import com.typesafe.scalalogging.LazyLogging
import concrete.heuristic.{Assign, BadDecision}
import concrete._

import scala.collection.mutable

class LastConflict(pm: ParameterManager, val pool: Seq[Variable]) extends VariableHeuristic with LazyLogging {
  private val K: Int = pm.getOrElse("lastconflict.size", 1)
  private val testingSet = new mutable.HashSet[Variable]()
  private var candidate: Option[Variable] = None

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

  override def toString: String = s"lc-$K"
}
