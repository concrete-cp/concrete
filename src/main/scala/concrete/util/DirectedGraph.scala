package concrete.util

import scala.collection.mutable

class DirectedGraph(val nbVertices: Int) {
  type Vertex = Int
  val nodes = new ScalaBitSet(nbVertices)
  val successors: Array[ScalaBitSet] = Array.fill(nbVertices)(new ScalaBitSet())
  val predecessors: Array[ScalaBitSet] = Array.fill(nbVertices)(new ScalaBitSet())

  def clear(): Unit = {
    for (i <- 0 until nbVertices) {
      successors(i).clear()
      predecessors(i).clear()
    }
  }

  def removeNode(x: Int): Boolean = {
    nodes.remove(x) && {
      for (s <- successors(x)) {
        predecessors(s).remove(x)
      }
      successors(x).clear()
      for (p <- predecessors(x)) {
        successors(p).remove(x)
      }
      predecessors(x).clear()
      true
    }
  }

  def removeArc(from: Int, to: Int): Boolean = {
    successors(from).contains(to) && {
      assert(predecessors(to)(from), "Incoherent predecessors")
      successors(from).remove(to) | predecessors(to).remove(from)
    }
  }

  def apply(from: Int, to: Int): Boolean = {
    assert(successors(from).contains(to) == predecessors(to).contains(from), "Graph is not coherent")
    successors(from).contains(to)
  }

  def hasNode(x: Int): Boolean = nodes.contains(x)

  override def toString: String = nodes.iterator
    .map {
      n => s"$n: ${successors(n)}"
    }
    .mkString("\n")

  def hasEdge(i: Int, j: Int): Boolean = successors(i).contains(j)

  def findAllSCC(): Array[Int] = {
    var index = 0
    var currentSCC = 0
    val stack = new BitSetStack(nbVertices)
    val indices = Array.fill(nbVertices)(-1)
    val lowLinks = new Array[Int](nbVertices)
    val scc = new Array[Int](nbVertices)

    for (v <- 0 until nbVertices) {
      if (indices(v) < 0) {
        strongConnect(v)
      }
    }

    def strongConnect(v: Int): Unit = {
      // Set the depth index for v to the smallest unused index
      indices(v) = index
      lowLinks(v) = index
      index += 1
      stack.push(v)

      // Consider successors of v
      for (w <- successors(v)) {
        if (indices(w) < 0) {
          // Successor w has not yet been visited; recurse on it
          strongConnect(w)
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
        var w = 0
        do {
          w = stack.pop()
          scc(w) = currentSCC
        } while (v != w)
        currentSCC += 1
      }
    }

    scc
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
      val n = vertex(i); val p = parent(n); var s = p

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


  def completeTree(tree: Array[Int], root: Int): Array[ScalaBitSet] = {
    ???
  }

  def addArc(from: Int, to: Int): Boolean = {
    assert(successors(from)(to) == predecessors(to)(from), "Graph is not coherent")
    addNode(from)
    addNode(to)
    successors(from).add(to) & predecessors(to).add(from)
  }

  def addNode(x: Int): Boolean = {
    nodes.add(x)
  }


}
