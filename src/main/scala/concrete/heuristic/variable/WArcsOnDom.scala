package concrete
package heuristic
package variable

import java.util.EventObject

class WArcsOnDom(params: ParameterManager, decisionVariables: Array[Variable]) extends VariableHeuristic(params, decisionVariables) {

  val arcs: Array[Array[Int]] = {
    val max = decisionVariables.map(_.id).max + 1

    Array.ofDim[Int](max, max)
  }

  def score(variable: Variable, dom: Domain, state: ProblemState) = {
    val id = variable.id
    var score = 0
    for (i <- 0 until id) {
      if (!state.domains(i).isAssigned) {
        score += arcs(i)(id)
      }
    }
    for (i <- id + 1 until arcs.length) {
      if (!state.domains(i).isAssigned) {
        score += arcs(id)(i)
      }
    }
    score.toDouble / dom.size
  }

  override def event(e: EventObject) = e match {
    case ContradictionEvent(c) =>
      for (v1 <- c.from) {
        val id1 = v1.id
        for (v2 <- c.to if v1 ne v2) {
          val id2 = v2.id

          if (id1 < id2) {
            arcs(id1)(id2) += 1
          } else {
            arcs(id2)(id1) += 1
          }

        }
      }
  }

}