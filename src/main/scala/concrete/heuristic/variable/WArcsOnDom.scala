package concrete
package heuristic
package variable

import java.util.EventObject

import scala.collection.{immutable, mutable}

object Arc {
  def apply(i1: Int, i2: Int): (Int, Int) = {
    if (i1 < i2) {
      (i1, i2)
    } else {
      (i2, i1)
    }
  }
}


class WArcsOnDom(val pool: Seq[Variable], tieBreaker: VariableHeuristic, val trustCandidates: Boolean = true)
  extends ScoredVariableHeuristic(tieBreaker) {

  private var arcs: Array[mutable.Map[Int, Int]] = _ // = new mutable.HashMap[(Int, Int), Int]().withDefaultValue(0)

  private var neighbors: immutable.Map[Int, Seq[Int]] = _

  override def compute(s: MAC, ps: ProblemState): ProblemState = {
    val max = s.problem.nbVariables + 1
    arcs = Array.fill(max)(new mutable.HashMap[Int, Int]().withDefaultValue(0))

    val neighb = new mutable.HashMap[Int, Set[Int]].withDefaultValue(Set())
    for {
      c <- s.problem.constraints
      Seq(v1, v2) <- c.scope.view.filterNot(ps.dom(_).isAssigned).map(_.id).combinations(2)
    } {
      neighb(v1) += v2
      neighb(v2) += v1
        arcs(v1)(v2) += 1
    }
    neighbors = neighb.mapValues(_.toSeq).toMap
    super.compute(s, ps)
  }

  def score(variable: Variable, dom: Domain, state: ProblemState): Double = {
    val id = variable.id
    var score = 0
    for (i <- neighbors(id)) {
      if (!state.domains(i).isAssigned) {
        score += arcs(i)(id) + arcs(id)(i) //Arc(i, id))
      }
    }
    score.toDouble / dom.size
  }

  override def event(e: EventObject): Unit = {
    e match {
      case ContradictionEvent(c) =>
        for (v1 <- c.from) {
          val id1 = v1.id
          if (id1 >= 0) {
            for (v2 <- c.to if v1 ne v2) {
              val id2 = v2.id
              if (id2 >= 0) {
                arcs(id1)(id2) += 1
              }
            }
          }
        }
      case _ =>
    }
    //    for {
    //      c <- PartialFunction.condOpt(e) {
    //        case ContradictionEvent(c) => c
    //      }
    //      v1 <- c.from
    //      id1 = v1.id
    //      if id1 >= 0
    //      v2 <- c.to if v1 ne v2
    //      id2 = v2.id
    //      if id2 >= 0
    //    } {
    //
    //    }
  }


  override def shouldRestart: Boolean = true

  override def toString = s"max-warcs/dom then $tieBreaker"
}