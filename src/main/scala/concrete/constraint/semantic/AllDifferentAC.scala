package concrete.constraint.semantic

import bitvectors.BitVector
import concrete._
import concrete.constraint.Constraint
import concrete.util.{DirectedGraph, IntIntMap, ScalaBitSet}

import scala.collection.mutable


class AllDifferentAC(scope: Array[Variable]) extends Constraint(scope) with AllDiffChecker {
  private val n = scope.length
  private val map = {
    val allValues = scope.flatMap(_.initDomain).distinct
    val m = new IntIntMap(allValues.length)
    for ((v, i) <- allValues.zipWithIndex) {
      m.justPut(v, i + n)
    }
    m
  }
  private val n2 = n + map.size
  private val digraph = new DirectedGraph(n2 + 1)
  private val free = new ScalaBitSet(n2)
  private val father = new Array[Int](n2)
  private val matching = Array.fill(n)(-1)
  // var i = 0

  def this(vars: Variable*) = this(vars.toArray)

  override def revise(problemState: ProblemState, modified: BitVector): Outcome = {
    findMaximumMatching(problemState)
      .andThen { ps =>
        for (i <- 0 until n) {
          matching(i) = if (digraph.pred(i).isEmpty) -1 else digraph.pred(i).head
        }
        filter(ps)
      }
  }

  private def filter(initState: ProblemState): Outcome = {

    var ps: ProblemState = initState
    val nodeSCC = buildSCC()

    for (i <- 0 until n) {
      val v = scope(i)
      ps = ps.dom(v).find { k => val j = map(k); nodeSCC(i) != nodeSCC(j) && matching(i) == j }
        .map { k =>
          assert(ps.dom(v).contains(k))
          ps.assign(v, k)
        }
        .getOrElse {
          val dom = ps.dom(v).filter { k =>
            val j = map(k)
            val b = nodeSCC(i) == nodeSCC(j)
            if (!b) digraph.removeArc(i, j)
            b
          }
          assert(dom.nonEmpty)
          ps.updateDomNonEmpty(v, dom)
        }
    }

    ps
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
    for (i <- 0 until n2) {
      digraph.succ(i).clear()
      digraph.pred(i).clear()
    }

    free.set(0, n2)

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
      (s, i) => if (free.contains(i)) tryToMatch(i, s) else s
    }

  }

  private def tryToMatch(i: Int, ps: ProblemState): Outcome = {
    val mate = augmentPathBFS(i)
    if (mate == -1) {
      Contradiction(scope)
    } else {
      free -= mate
      free -= i
      var tmp = mate
      while (tmp != i) {
        digraph.removeArc(father(tmp), tmp)
        digraph.addArc(tmp, father(tmp))
        tmp = father(tmp)
      }
      ps
    }
  }

  private def augmentPathBFS(root: Int): Int = {
    val in = new ScalaBitSet(n2)
    val fifo = mutable.Queue[Int](root)
    while (fifo.nonEmpty) {
      val x = fifo.dequeue()
      // Optimized for profiler
      val it = digraph.succ(x).iterator
      while (it.hasNext) {
        val y = it.next()
        if (!in.contains(y)) {
          father(y) = x
          fifo.enqueue(y)
          in += y
          if (free.contains(y)) return y
        }
      }
    }

    -1
  }

  override def init(ps: ProblemState): Outcome = ps

  override def simpleEvaluation: Int = 3

  override def except: Set[Int] = Set()

  override protected def advise(problemState: ProblemState, event: Event, pos: Int): Int = arity * arity
}
