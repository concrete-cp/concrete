package concrete.constraint.semantic

import bitvectors.BitVector
import concrete._
import concrete.constraint.Constraint
import concrete.util.{DirectedGraph, IntIntMap}

import scala.collection.immutable.Queue
import scala.collection.{immutable, mutable}


class AllDifferentAC(scope: Array[Variable]) extends Constraint(scope) with AllDiffChecker {
  private val n = scope.length
  private val map = {
    val allValues = scope.flatMap(_.initDomain).distinct
    allValues.zipWithIndex
      .foldLeft(new IntIntMap(allValues.length)) {
        case (m, (v, i)) => m.addOne(v -> (i + n))
      }
  }
  private val n2 = n + map.size
  private val digraph = new DirectedGraph(n2 + 1)
  private val free = new mutable.BitSet(n2)
  private val father = new Array[Int](n2)
  private val matching = Array.fill(n)(-1)

  private val full = (0 until n2).to(immutable.BitSet)
  // var i = 0

  def this(vars: Variable*) = this(vars.toArray)

  override def revise(problemState: ProblemState, modified: BitVector): Outcome = {
    findMaximumMatching(problemState)
      .andThen { ps =>
        for (i <- 0 until n) {
          matching(i) = digraph.predecessors(i).headOption.getOrElse(-1)
        }
        filter(ps)
      }
  }

  override def init(ps: ProblemState): Outcome = ps

  override def simpleEvaluation: Int = 3

  override def except: Set[Int] = Set()

  override protected def advise(problemState: ProblemState, event: Event, pos: Int): Int = arity * arity

  private def filter(initState: ProblemState): Outcome = {
    val nodeSCC = buildSCC()

    (0 until n).foldLeft(initState) { (ps, i) =>
      val v = scope(i)
      val dom = ps.dom(v)
      val newDom = dom.find { k =>
        val j = map(k); nodeSCC(i) != nodeSCC(j) && matching(i) == j
      } match {
        case Some(k) =>
          assert(dom.contains(k))
          dom.assign(k)
        case None =>
          dom.filter { k =>
            val j = map(k)
            val b = nodeSCC(i) == nodeSCC(j)
            if (!b) digraph.removeArc(i, j)
            b
          }
      }

      ps.updateDomNonEmpty(v, newDom)
    }

  }

  private def buildSCC(): Array[Int] = {
    if (n2 > n * 2) {
      digraph.removeNode(n2)
      digraph.addNode(n2)
      for (i <- n until n2) {
        if (free.contains(i)) digraph.addArc(i, n2) else digraph.addArc(n2, i)
      }
    }
    val scc = digraph.findAllSCC()
    digraph.removeNode(n2)
    scc
  }

  private def findMaximumMatching(ps: ProblemState): Outcome = {
    digraph.clear()
    free |= full //set(0, n2)

    for (i <- 0 until n) {
      val mate = matching(i)
      for (k <- ps.dom(scope(i))) {
        val j = map(k)
        if (mate == j) {
          assert(free(i) && free(j))
          digraph.addArc(j, i)
          free -= i
          free -= j
        } else {
          digraph.addArc(i, j)
        }
      }
    }

    ps.fold(0 until n) {
      (s, i) => if (free.contains(i) && !tryToMatch(i, s)) Contradiction(scope) else s
    }

  }

  private def tryToMatch(i: Int, ps: ProblemState): Boolean = {
    val mate = augmentPathBFS(i, digraph.successors(i).iterator)
    mate >= 0 && {
      free -= mate
      free -= i
      removePath(mate, i, digraph, father)
    }
  }

  /**
    * @param from
    * @param to
    * @param digraph
    * @param paths
    * @return true
    */
  private def removePath(from: Int, to: Int, digraph: DirectedGraph, paths: Array[Int]): Boolean = {
    from == to || {
      digraph.removeArc(father(from), from)
      digraph.addArc(from, father(from))
      removePath(father(from), to, digraph, paths)
    }
  }

  private def augmentPathBFS(x: Int,
                             vertices: Iterator[Int],
                             fifo: Queue[Int] = Queue(),
                             in: mutable.BitSet = new mutable.BitSet(n2)): Int = {

    if (vertices.hasNext) {
      val y = vertices.next()
      if (in.contains(y)) {
        augmentPathBFS(x, vertices, fifo, in)
      } else {
        father(y) = x
        in += y
        if (free(y)) {
          y
        } else {
          augmentPathBFS(x, vertices, fifo.enqueue(y), in)
        }
      }
    } else if (fifo.isEmpty) {
      -1
    } else {
      val (next, dq) = fifo.dequeue
      augmentPathBFS(next, digraph.successors(next).iterator, dq, in)
    }

  }
}
