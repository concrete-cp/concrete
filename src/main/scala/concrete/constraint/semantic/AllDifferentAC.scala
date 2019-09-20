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
  //  {
  //    _.addOne(_)
  //  }


  private val n2 = n + map.size
  private val free = new mutable.BitSet(n2)
  private val father = new Array[Int](n2)
  private val full = (0 until n2).to(immutable.BitSet)
  private val matching = Array.fill[Option[Int]](n)(None)
  private var digraph = new DirectedGraph() //n2 + 1)
  // var i = 0

  def this(vars: Variable*) = this(vars.toArray)

  override def revise(problemState: ProblemState, modified: BitVector): Outcome = {
    findMaximumMatching(problemState, modified)
      .andThen(filter)
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

  private def filter(initState: ProblemState): Outcome = {

    for (i <- 0 until n) {
      matching(i) = digraph.pred(i).headOption
    }

    val nodeSCC = buildSCC()

    (0 until n).foldLeft(initState) { (ps, i) =>
      val v = scope(i)
      val dom = ps.dom(v)
      val newDom =
        dom.find { k =>
          val j = map(k); nodeSCC(i) != nodeSCC(j) && matching(i).contains(j)
        } match {
          case Some(k) =>
            assert(dom.contains(k))
            dom.assign(k)
          case None =>
            dom.filter { k =>
              val j = map(k)
              val b = nodeSCC(i) == nodeSCC(j)
              // if (!b) digraph = digraph.removeArc(i, j)
              b
            }
        }

      ps.updateDomNonEmpty(v, newDom)
    }

  }

  private def buildSCC(): Array[Int] = {
    if (n2 > n * 2) {
      // digraph = digraph.removeNode(n2)
      // digraph.addNode(n2)
      for (i <- n until n2) {
        digraph = if (free.contains(i)) digraph.addEdge(i, n2) else digraph.addEdge(n2, i)
      }
    }
    val scc = digraph.findAllSCC()
    digraph = digraph.removeNode(n2)
    scc
  }

  private def findMaximumMatching(ps: ProblemState, mod: BitVector): Outcome = {
    digraph = ps(this)
    free |= full //set(0, n2)

    for (i <- mod) {
      val dom = ps.dom(scope(i))
      digraph = digraph.filterSucc(i, j => dom.contains(revMap(j)))

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
    }

    ps.updateState(this, digraph).andThen { ps =>
      for (i <- 0 until n; mate <- matching(i)) {
        if (ps.dom(scope(i)).contains(revMap(mate))) {
          digraph = digraph.removeEdge(i, mate).addEdge(mate, i)
          free -= i
          free -= mate
        }
      }

      ps.fold(0 until n) {
        (s, i) => if (free.contains(i) && !tryToMatch(i, s)) Contradiction(scope) else s
      }
    }

  }

  private def tryToMatch(i: Int, ps: ProblemState): Boolean = {
    val mate = augmentPathBFS(i, digraph.succ(i).iterator)
    mate >= 0 && {
      free -= mate
      free -= i
      digraph = removePath(mate, i, digraph, father)
      true
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
      augmentPathBFS(next, digraph.succ(next).iterator, dq, in)
    }

  }
}
