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

object ArcsScores {
  def apply(max: Int): ArcsScores = if (max < 10000) new ArrayArcsScores(max) else new MapArcsScores(max)
}

trait ArcsScores {
  def apply(i: Int, j: Int): Int

  def increment(i: Int, j: Int): Unit
}

class ArrayArcsScores(max: Int) extends ArcsScores {
  private val arcs = Array.ofDim[Int](max, max)

  def apply(i: Int, j: Int): Int = arcs(i)(j) + arcs(j)(i)

  def increment(i: Int, j: Int): Unit = arcs(i)(j) += 1
}

class MapArcsScores(max: Int) extends ArcsScores {
  private val arcs = Array.fill(max)(new mutable.HashMap[Int, Int]().withDefaultValue(0))

  def apply(i: Int, j: Int): Int = arcs(i)(j) + arcs(j)(i)

  def increment(i: Int, j: Int): Unit = arcs(i)(j) += 1
}

class WArcsOnDom(val pool: Seq[Variable])
  extends ScoredVariableHeuristic {

  private var arcs: ArcsScores = _ // = new mutable.HashMap[(Int, Int), Int]().withDefaultValue(0)

  private var neighbors: immutable.Map[Int, Seq[Int]] = _

  override def compute(s: MAC, ps: ProblemState): ProblemState = {
    val max = s.problem.nbVariables + 1
    arcs = ArcsScores(max)

    val neighb = new mutable.HashMap[Int, mutable.Set[Int]] //.withDefaultValue(new mutable.HashSet())
    for {
      c <- s.problem.constraints
      scope = c.scope.filterNot(ps.dom(_).isAssigned).map(_.id)
      i <- scope.indices
    } {
      val v1 = scope(i)
      for (j <- i + 1 until scope.length) {
        val v2 = scope(j)
        neighb.getOrElseUpdate(v1, new mutable.HashSet()) += v2
        neighb.getOrElseUpdate(v2, new mutable.HashSet()) += v1
        arcs.increment(v1, v2)
      }
    }
    neighbors = neighb.view.mapValues(_.toSeq).toMap
    ps //super.compute(s, ps)
  }

  def score(variable: Variable, dom: Domain, state: ProblemState): Double = {
    val id = variable.id
    var score = 0
    for (i <- neighbors(id)) {
      if (!state.dom(i).isAssigned) {
        score += arcs(i, id)
      }
    }
    score.toDouble / dom.size
  }

  override def event[S <: Outcome](e: EventObject, ps: S): S = {
    if (e == ContradictionEvent) {
      val c = ps.asInstanceOf[Contradiction]
      for (v1 <- c.from) {
        val id1 = v1.id
        if (id1 >= 0) {
          for (v2 <- c.to if v1 ne v2) {
            val id2 = v2.id
            if (id2 >= 0) {
              arcs.increment(id1, id2)
            }
          }
        }
      }
    }
    ps
  }


  override def shouldRestart: Boolean = true

  override def toString = s"max-warcs/dom"
}