package concrete.constraint.semantic

import bitvectors.BitVector
import concrete._
import concrete.constraint.{Constraint, StatefulConstraint}
import concrete.util.{DirectedGraph, IntIntMap}

import scala.collection.immutable.Queue
import scala.collection.{immutable, mutable}


class AllDifferentAC(scope: Array[Variable]) extends Constraint(scope) with AllDiffChecker
  with StatefulConstraint[DirectedGraph] {

  private val n = scope.length
  private val map = {
    val allValues = scope.flatMap(_.initDomain).distinct
    allValues.zipWithIndex
      .foldLeft(new IntIntMap(allValues.length)) {
        case (m, (v, i)) => m.addOne(v -> (i + n))
      }
  }

  private val revMap = map.iterator.map(_.swap).to(IntIntMap)


  private val n2 = n + map.size
  private val father = new Array[Int](n2)
  private val full = (0 until n2).to(immutable.BitSet)
  private val matching = Array.fill[Option[Int]](n)(None)

  def this(vars: Variable*) = this(vars.toArray)

  override def revise(problemState: ProblemState, modified: BitVector): Outcome = {
    val digraph = updatedGraph(problemState, modified)
    val (ps, free, matedGraph) = findMaximumMatching(problemState, modified, digraph)
    ps.andThen(filter(_, free, matedGraph))
      .andThen { mod =>
        var updGraph = digraph
        for (m <- 0 until arity) {
          val x = scope(m)
          val dom = mod.dom(x)
          if (dom.size < ps.dom(x).size) {
            updGraph = updGraph.filterSucc(m, i => dom.contains(revMap(i)))
          }
        }
        mod.updateState(this, updGraph)
      }
  }

  override def init(ps: ProblemState): Outcome = {
    val arcs = for (i <- 0 until n; k <- ps.dom(scope(i))) yield {
      (i, map(k))
    }

    val digraph = arcs.foldLeft(new DirectedGraph()) { case (dg, (i, j)) => dg.addEdge(i, j) }

    ps.updateState(this, digraph)
  }

  override def simpleEvaluation: Int = 3

  override def except: Set[Int] = Set()

  override protected def advise(problemState: ProblemState, event: Event, pos: Int): Int =
    arity * arity

  private def filter(initState: ProblemState, free: mutable.BitSet, digraph: DirectedGraph): Outcome = {

    for (i <- 0 until n) {
      matching(i) = digraph.pred(i).headOption
    }

    val nodeSCC = buildSCC(digraph, free)


    Iterator.range(0, n).foldLeft(initState) { (ps, i) =>
      val x = scope(i)
      val dom = ps.dom(x)
      val newDom =
        dom.find { k =>
          val j = map(k); nodeSCC(i) != nodeSCC(j) && matching(i).contains(j)
        } match {
          case Some(k) =>
            assert(dom.contains(k))
            dom.assign(k)
          case None =>
            dom.filter(k => nodeSCC(i) == nodeSCC(map(k)))
        }

      ps.updateDomNonEmpty(x, newDom)
    }

  }

  private def buildSCC(digraph: DirectedGraph, free: mutable.BitSet): Array[Int] = {
    var updatedGraph = digraph
    if (n2 > n * 2) {
      // digraph = digraph.removeNode(n2)
      // digraph.addNode(n2)
      for (i <- n until n2) {
        updatedGraph = if (free.contains(i)) updatedGraph.addEdge(i, n2) else updatedGraph.addEdge(n2, i)
      }
    }
    updatedGraph.findAllSCC()
  }

  private def updatedGraph(ps: ProblemState, modified: BitVector) = {
    modified.foldLeft(ps(this)) { (graph, m) =>
      val dom = ps.dom(scope(m))
      graph.filterSucc(m, i => dom.contains(revMap(i)))
    }
  }


  private def findMaximumMatching(
                                   ps: ProblemState,
                                   mod: BitVector,
                                   digraph: DirectedGraph): (Outcome, mutable.BitSet, DirectedGraph) = {
    val free = new mutable.BitSet() |= full //set(0, n2)

    // val mate = matching(i)
    //        for (k <- ps.dom(scope(i))) {
    //          val j = map(k)
    //          //        if (mate == j) {
    //          //          assert(free(i) && free(j))
    //          //          digraph = digraph.addArc(j, i)
    //          //          free -= i
    //          //          free -= j
    //          //        } else {
    //          digraph = digraph.addArc(i, j)
    //          //        }
    //        }

    var matched = digraph
    for (i <- 0 until n; mate <- matching(i)) {
      if (ps.dom(scope(i)).contains(revMap(mate))) {
        matched = matched.removeEdge(i, mate).addEdge(mate, i)
        free -= i
        free -= mate
      }
    }

    val mod = ps.fold(0 until n) { (s, i) =>
      if (free.contains(i)) {
        tryToMatch(i, s, free, matched) match {
          case Some(mtch) =>
            matched = mtch
            s
          case None => Contradiction(scope)
        }
      } else {
        s
      }
    }

    (mod, free, matched)

  }

  private def tryToMatch(i: Int, ps: ProblemState,
                         free: mutable.BitSet, digraph: DirectedGraph): Option[DirectedGraph] = {
    val mate = augmentPathBFS(i, digraph.succ(i).iterator, free, digraph)
    if (mate >= 0) {
      free -= mate
      free -= i
      Some(removePath(mate, i, digraph, father))
    } else {
      None
    }
  }

  /**
    * @param from
    * @param to
    * @param digraph
    * @param paths
    * @return true
    */
  @scala.annotation.tailrec
  private def removePath(from: Int, to: Int, digraph: DirectedGraph, paths: Array[Int]): DirectedGraph = {
    if (from == to) {
      digraph
    } else {
      removePath(father(from), to,
        digraph.removeEdge(father(from), from).addEdge(from, father(from)),
        paths)
    }
  }

  @scala.annotation.tailrec
  private def augmentPathBFS(
                              x: Int,
                              vertices: Iterator[Int],
                              free: mutable.BitSet,
                              digraph: DirectedGraph,
                              fifo: Queue[Int] = Queue(),
                              in: mutable.BitSet = new mutable.BitSet(n2)): Int = {

    if (vertices.hasNext) {
      val y = vertices.next()
      if (in.contains(y)) {
        augmentPathBFS(x, vertices, free, digraph, fifo, in)
      } else {
        father(y) = x
        in += y
        if (free(y)) {
          y
        } else {
          augmentPathBFS(x, vertices, free, digraph, fifo.enqueue(y), in)
        }
      }
    } else if (fifo.isEmpty) {
      -1
    } else {
      val (next, dq) = fifo.dequeue
      augmentPathBFS(next, digraph.succ(next).iterator, free, digraph, dq, in)
    }

  }
}
