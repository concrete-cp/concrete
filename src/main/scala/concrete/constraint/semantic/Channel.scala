package concrete
package constraint
package semantic

import java.util

import bitvectors.BitVector
import cspom.compiler.QueueSet

import scala.collection.mutable

/**
  * Constraint x(i) = j <=> x(j) = i
  */
final class Channel(x: Array[Variable], offset: Int) extends Constraint(x)
  with StatefulConstraint[Map[Int, Set[Int]]] {

  def advise(problemState: ProblemState, event: Event, pos: Int): Int = {
    val fx = problemState(this)
    val card = fx.size
    card * card
  }

  def check(tuple: Array[Int]): Boolean = {
    x.indices.forall { i =>
      val j = tuple(i)
      tuple(j - offset) == i + offset
    }
  }

  def init(ps: ProblemState): Outcome =
    ps.fold(x) { (ps, v) =>
      ps.shaveDom(v, offset, offset + x.length - 1)
    }
      .andThen { ps =>

        ps.updateState(this, computePredecessors(ps))
      }

  def computePredecessors(ps: ProblemState): Map[Int, Set[Int]] = {
    val pred = new mutable.HashMap[Int, Set[Int]].withDefaultValue(Set())
    for (p <- x.indices if !ps.dom(p).isAssigned; v <- ps.dom(p)) {
      pred(v) += p + offset
    }
    pred.toMap
  }

  def revise(problemState: ProblemState, mod: BitVector): Outcome = {
    //println(s"$mod of ${toString(problemState)}")
    // fx contains "free variables"

    val queue = new QueueSet(util.BitSet.valueOf(mod.words))
    var pred = problemState(this)
    var ps = problemState: Outcome

    while (queue.nonEmpty && ps.isState) {
      val m = queue.dequeue()
      val dom = ps.dom(x(m))

      for (p <- pred.getOrElse(m + offset, Iterator.empty)) {
        if (!dom.contains(p)) {
          val mod = ps.remove(x(p - offset), m + offset)
          if (mod ne ps) {
            queue.enqueue(p - offset)
            ps = mod
          }
          pred = pred.updated(p, pred(p) - (m + offset))
            .updated(m + offset, pred(m + offset) - p)
        }
      }
      if (dom.isAssigned) {
        val v = dom.singleValue
        val mod = ps.tryAssign(x(v - offset), m + offset)
        if (mod ne ps) {
          queue.enqueue(v - offset)
          ps = mod
        }
        pred -= m + offset
      }
    }

    assert(ps.isState || pred == computePredecessors(ps.toState))

    ps.updateState(this, pred)
    //
    //    println(toString(problemState) + " " + mod)
    //
    //    var fx = problemState(this)
    //
    //    // Check whether variables have been assigned
    //    problemState.fold(mod) { (ps, p) =>
    //      val dom = ps.dom(x(p))
    //      if (dom.isAssigned) {
    //        fx -= p
    //
    //        val v = dom.singleValue
    //
    //        ps.tryAssign(x(v - offset), p + offset)
    //      } else {
    //        ps
    //      }
    //    }
    //      .fold(fx) { (ps, p) =>
    //
    //        val r = ps.filterDom(x(p))(v => ps.dom(x(v - offset)).contains(p + offset))
    //        println(x(p).toString(ps) + " -> " + x(p).toString(r.toState))
    //        r
    //      }
    //      .updateState(this, fx)

  }

  override def toString(ps: ProblemState): String = {
    s"channel $offset/${x.map(ps.dom(_).toString).mkString(", ")} fx = ${ps(this)}"
  }

  def simpleEvaluation: Int = 2
}