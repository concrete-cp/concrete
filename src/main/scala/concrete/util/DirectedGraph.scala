package concrete.util

class DirectedGraph(val n: Int) {
  val nodes = new ScalaBitSet(n)
  private val successors = Array.fill(n)(new ScalaBitSet())
  private val predecessors = Array.fill(n)(new ScalaBitSet())

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

  def addArc(from: Int, to: Int): Boolean = {
    assert(successors(from)(to) == predecessors(to)(from), "Graph is not coherent")
    addNode(from)
    addNode(to)
    successors(from).add(to) & predecessors(to).add(from)
  }

  def addNode(x: Int): Boolean = {
    nodes.add(x)
  }

  def hasNode(x: Int): Boolean = nodes.contains(x)

  def succ(x: Int): ScalaBitSet = successors(x)

  def pred(x: Int): ScalaBitSet = predecessors(x)

  override def toString: String = nodes.iterator
    .map {
      n => s"$n: ${successors(n)}"
    }
    .mkString("\n")

  def hasEdge(i: Int, j: Int): Boolean = successors(i).contains(j)

  def findAllSCC(): Array[Int] = {
    var index = 0
    var currentSCC = 0
    val stack = new BitSetStack(n)
    val indices = Array.fill(n)(-1)
    val lowLinks = new Array[Int](n)
    val scc = new Array[Int](n)

    for (v <- 0 until n) {
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
}
