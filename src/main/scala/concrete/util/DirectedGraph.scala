package concrete.util

import scala.collection.mutable

final class DirectedGraph(
                           private val successors: Vector[Set[Int]] = Vector.empty,
                           private val predecessors: Vector[Set[Int]] = Vector.empty
                         ) {

  //  val successors: Array[mutable.BitSet] = Array.fill(nbVertices)(new mutable.BitSet())
  //  val predecessors: Array[mutable.BitSet] = Array.fill(nbVertices)(new mutable.BitSet())

  private val nbVertices: Int = math.max(successors.size, predecessors.size)

  def findAllSCC(): Array[Int] = findAllSCCIterative()

  private def findAllSCCIterative(): Array[Int] = {

    val stack = new BitSetStack(nbVertices)
    val indices = Array.fill(nbVertices)(-1)
    val lowLinks = new Array[Int](nbVertices)
    val scc = new SCC()
    val iterators = new Array[Iterator[Int]](nbVertices)
    var index = 0
    val exploStack = new mutable.Stack[Int]()


    for (u <- 0 until nbVertices if indices(u) < 0) {
      var v = u
      var continue = true
      while (continue) {

        if (iterators(v) == null) {
          indices(v) = index
          lowLinks(v) = index
          index += 1
          stack.push(v)
          iterators(v) = succ(v).iterator
        }
        if (iterators(v).hasNext) {
          val w = iterators(v).next()
          if (indices(w) < 0) {
            exploStack.push(v).push(w)
            v = w
          } else if (stack.contains(w)) {
            lowLinks(v) = math.min(lowLinks(v), indices(w))
          }
        } else {
          if (lowLinks(v) == indices(v)) {
            scc.add(v, stack)
          }
          if (exploStack.isEmpty) {
            continue = false
          } else {
            val w = exploStack.pop()
            v = exploStack.pop()
            lowLinks(v) = math.min(lowLinks(v), lowLinks(w))
          }
        }
      }
    }

    scc.scc
  }

  def succ(v: Int): Set[Int] = if (v < successors.length) successors(v) else Set.empty

  def removeNode(x: Int): DirectedGraph = {
    val newPred = succ(x).foldLeft(predecessors) { (p, v) =>
      p.updated(v, p(v) - x)
    }
    val newSucc = pred(x).foldLeft(successors) { (s, v) =>
      s.updated(v, s(v) - x)
    }
    new DirectedGraph(
      if (x < newSucc.length) newSucc.updated(x, Set.empty) else newSucc,
      if (x < newPred.length) newPred.updated(x, Set.empty) else newPred
    )

    //    for (s <- successors(x)) {
    //      predecessors(s).remove(x)
    //    }
    //    successors(x).clear()
    //    for (p <- predecessors(x)) {
    //      successors(p).remove(x)
    //    }
    //    predecessors(x).clear()
    //    true

  }

  //  def clear(): Unit = {
  //    for (i <- 0 until nbVertices) {
  //      successors(i).clear()
  //      predecessors(i).clear()
  //    }
  //  }

  def pred(v: Int): Set[Int] = if (v < predecessors.length) predecessors(v) else Set.empty

  def removeEdge(from: Int, to: Int): DirectedGraph = {
    if (hasEdge(from, to)) {
      new DirectedGraph(
        successors.updated(from, successors(from) - to),
        predecessors.updated(to, predecessors(to) - from)
      )
      //
      //      successors(from).remove(to) | predecessors(to).remove(from)
    } else {
      this
    }
  }

  def hasEdge(i: Int, j: Int): Boolean = succ(i).contains(j)

  def setSucc(from: Int, newSucc: Set[Int]): DirectedGraph = {
    if (succ(from).size == newSucc.size) {
      this
    } else {
      assume(newSucc.subsetOf(succ(from)))
      var newPred = predecessors
      for (r <- succ(from) if !newSucc(r)) {
        newPred = newPred.updated(r, newPred(r) - from)
      }
      new DirectedGraph(successors.updated(from, newSucc), newPred)
    }
  }

  def filterSucc(from: Int, f: Int => Boolean): DirectedGraph = {
    val newSucc = succ(from).filter(f)
    setSucc(from, newSucc)
  }

  // def hasNode(x: Int): Boolean = nodes.contains(x)
  //
  override def toString: String = s"Directed graph with ${successors.size} vertices and ${successors.map(_.size).sum} edges"

//    successors.zipWithIndex.filter(_._1.nonEmpty)
//    .map {
//      case (succ, n) => s"$n: $succ"
//    }
//    .mkString("\n")

  def findAllSCCRecursive(): Array[Int] = {

    val stack = new BitSetStack(nbVertices)
    val indices = Array.fill(nbVertices)(-1)
    val lowLinks = new Array[Int](nbVertices)
    val scc = new SCC()

    Iterator.range(0, nbVertices).filter(indices(_) < 0)
      .foldLeft[Int](0) { case (index, v) =>
      strongConnect(v, index, scc, indices, lowLinks, stack)
    }

    scc.scc
  }

  // Lifted from "Modern Compiler Implementation in Java", 2nd ed. chapter 19.2
  def computeDominatorTree(root: Int): Array[Int] = {

    val bucket = Array.fill(nbVertices)(new mutable.HashSet[Int]())
    val dfnum = Array.fill(nbVertices)(-1)
    val vertex = Array.fill(nbVertices)(-1)
    val parent = Array.fill(nbVertices)(-1)
    val semi = Array.fill(nbVertices)(-1)
    val ancestor = Array.fill(nbVertices)(-1)
    val idom = Array.fill(nbVertices)(-1)
    val samedom = Array.fill(nbVertices)(-1)
    val best = Array.fill(nbVertices)(-1)

    def dfs(p: Int = -1, n: Int = root, N: Int = 0): Int = {
      if (dfnum(n) < 0) {
        dfnum(n) = N
        vertex(N) = n
        parent(n) = p
        var k = N + 1
        for (w <- successors(n)) {
          k = dfs(n, w, k)
        }
        k
      } else {
        N
      }
    }

    def ancestorWithLowestSemi(v: Int): Int = {
      val a = ancestor(v)
      if (ancestor(a) >= 0) {
        val b = ancestorWithLowestSemi(a)
        ancestor(v) = ancestor(a)
        if (dfnum(semi(b)) < dfnum(semi(best(v)))) {
          best(v) = b
        }
      }
      best(v)
    }

    def link(p: Int, n: Int): Unit = {
      ancestor(n) = p
      best(n) = n
    }

    val N = dfs()

    for (i <- (N - 1) until 0 by -1) {
      val n = vertex(i)
      val p = parent(n)
      var s = p

      for (v <- predecessors(n)) {
        val sPrime = if (dfnum(v) <= dfnum(n)) {
          v
        } else {
          semi(ancestorWithLowestSemi(v))
        }
        if (dfnum(sPrime) < dfnum(s)) {
          s = sPrime
        }
      }

      semi(n) = s
      bucket(s) += n
      link(p, n)

      for (v <- bucket(p)) {
        val y = ancestorWithLowestSemi(v)
        if (semi(y) == semi(v)) {
          idom(v) = p
        } else {
          samedom(v) = y
        }
      }
      bucket(p).clear()
    }

    for (i <- 0 until N) {
      val n = vertex(i)
      if (samedom(n) >= 0) {
        idom(n) = idom(samedom(n))
      }
    }
    idom
  }

  def addEdge(from: Int, to: Int): DirectedGraph = {
    // assert(successors(from)(to) == predecessors(to)(from), "Graph is not coherent")
    val newSucc = if (from >= successors.length) successors.padTo(from + 1, Set.empty) else successors
    val newPred = if (to >= predecessors.length) predecessors.padTo(to + 1, Set.empty) else predecessors
    new DirectedGraph(
      newSucc.updated(from, newSucc(from) + to),
      newPred.updated(to, newPred(to) + from)
    )
    //successors(from).add(to) & predecessors(to).add(from)
  }


  private def strongConnect(v: Int, initIndex: Int, scc: SCC, indices: Array[Int], lowLinks: Array[Int], stack: BitSetStack): Int = {

    // Set the depth index for v to the smallest unused index
    indices(v) = initIndex
    lowLinks(v) = initIndex
    var currentIndex = initIndex
    stack.push(v)

    // Consider successors of v
    for (w <- successors(v)) {
      if (indices(w) < 0) {
        // Successor w has not yet been visited; recurse on it
        currentIndex = strongConnect(w, currentIndex + 1, scc, indices, lowLinks, stack)
        lowLinks(v) = math.min(lowLinks(v), lowLinks(w))
      } else if (stack.contains(w)) {

        // Successor w is in stack S and hence in the current SCC
        // If w is not on stack, then (v, w) is a cross-edge in the DFS tree and must be ignored
        // Note: The next line may look odd - but is correct.
        // It says w.index not w.lowlink; that is deliberate and from the original paper
        lowLinks(v) = math.min(lowLinks(v), indices(w))

      }
    }

    // If v is a root node, pop the stack and generate an SCC
    if (lowLinks(v) == indices(v)) {
      scc.add(v, stack)
    }

    currentIndex
  }

  private class SCC {
    val scc = new Array[Int](nbVertices)
    private var currentSCC = 0

    def add(end: Int, stack: BitSetStack): Unit = {
      var w = 0
      do {
        w = stack.pop()
        scc(w) = currentSCC
      } while (w != end)
      currentSCC += 1
    }
  }


}
