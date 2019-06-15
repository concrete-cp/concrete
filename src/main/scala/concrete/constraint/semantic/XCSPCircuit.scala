package concrete.constraint.semantic

import java.util

import bitvectors.BitVector
import concrete.{Contradiction, Event, Outcome, ProblemState, Variable}
import concrete.constraint.Constraint

import scala.collection.mutable

class XCSPCircuit(scope: Array[Variable], start: Int, size: Variable) extends Constraint(scope :+ size) {
  override def revise(ps: ProblemState, modified: BitVector): Outcome = {
    // Distance from start to any node
    val distances = dijkstra(i => ps.dom(scope(i)), start)

    if (!distances.contains(start)) {
      Contradiction(scope)
    } else {

      val transposed = new mutable.HashMap[Int, mutable.Set[Int]]()
      for (i <- scope.indices; e <- ps.dom(scope(i))) {
        transposed.getOrElseUpdate(e, new mutable.HashSet()) += i
      }

      // Distance from any node to start
      val reverse = dijkstra(transposed.withDefaultValue(mutable.Set.empty), start)

      // Distance from start to start is shortest possible cycle
      assert(distances(start) == reverse(start), s"${distances(start)} should be the same as ${reverse(start)}")

      // Nodes in possible cycles must have both defined distance and reverse lengths
      val reachable = distances.keySet & reverse.keySet

      ps.shaveDom(size, distances(start), reachable.size)
        .andThen { ps =>
          val ub = ps.dom(size).last
          var o = ps: Outcome

          for (i <- scope.indices if i != start) {
            // Computes distance from start to i, and back
            val round = for (forward <- distances.get(i); backwards <- reverse.get(i)) yield {
              forward + backwards
            }

            // If distance higher than ub, put vertex out of circuit (round = None
            // means that vertex i is unreachable)
            if (round.forall(_ > ub)) {
              o = o.tryAssign(scope(i), i)
            }
          }
          o
        }
    }
  }

  private def dijkstra(edges: Int => Iterable[Int], start: Int): mutable.Map[Int, Int] = {

    val distances = new mutable.HashMap[Int, Int]()

    val queue = new util.ArrayDeque[Int]()

    for (v <- edges(start)) {
      distances(v) = 1
      if (v != start) queue.offer(v)
    }

    while (!queue.isEmpty) {
      val current = queue.poll()
      val distance: Int = distances(current)
      for (v <- edges(current)) {
        if (!distances.contains(v)) {
          distances(v) = distance + 1
          queue.offer(v)
        }
      }
    }

    distances

  }

  override def init(ps: ProblemState): Outcome = ps

  /**
    * @return true iff the constraint is satisfied by the given tuple
    */
  override def check(tuple: Array[Int]): Boolean = {
    // Visit cycle
    val visited = new util.BitSet()
    var current = start
    while (!visited.get(current)) {
      visited.set(current)
      current = tuple(current)
    }

    // Vertices not in the cycle should be self-references
    var i = visited.previousClearBit(scope.length - 1)
    while (i >= 0) {
      if (tuple(i) != i) return false
      i = visited.previousClearBit(i - 1)
    }

    // Check cycle length
    visited.cardinality == tuple(scope.length)
  }

  override def simpleEvaluation: Int = ???

  override protected def advise(problemState: ProblemState, event: Event, pos: Int): Int = 2 * arity
}
