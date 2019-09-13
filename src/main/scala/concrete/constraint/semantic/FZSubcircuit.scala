package concrete.constraint.semantic

import bitvectors.BitVector
import concrete._
import concrete.constraint.Constraint
import concrete.util.DirectedGraph

/**
  * Allows at most one subcircuit
  *
  * @param scope
  */
final class FZSubcircuit(scope: Array[Variable], offset: Int = 1) extends Constraint(scope) {
  //  private val diGraph = new DirectedGraph(arity)
  var card: Int = _

  override def revise(ps: ProblemState, modified: BitVector): Outcome = {
    val directedGraph = buildGraph(ps)

    filterSCC(directedGraph, ps)
  }

  /**
    * Filtering based on strong connected components
    * If one SCC contains a mandatory cycle, all other SCC are out of the cycles.
    *
    * @param directedGraph
    * @param ps
    * @return
    */
  private def filterSCC(directedGraph: DirectedGraph, ps: ProblemState): Outcome = {
    val scc = directedGraph.findAllSCC()
    val mandatoryCycles = findMandatoryCycles(directedGraph, scc)
    if (mandatoryCycles.isEmpty) {
      ps
    } else if (mandatoryCycles.size == 1) {
      val mc = mandatoryCycles.head
      ps.fold(0 until arity) { (state, p) =>
        if (scc(p) == mc) {
          state
        } else {
          state.tryAssign(scope(p), p + offset)
        }
      }
    } else {
      Contradiction((0 until arity).filter(i => mandatoryCycles.contains(scc(i))).map(scope(_)))
    }
  }

  def findMandatoryCycles(diGraph: DirectedGraph, scc: Array[Int]): Set[Int] = {
    Iterator.range(0, arity)
      .filterNot(i => diGraph.succ(i).contains(i))
      .map(scc(_))
      .toSet
  }

  def buildGraph(ps: ProblemState): DirectedGraph = {
    var diGraph = new DirectedGraph() //.clear()
    for (p <- 0 until arity; v <- ps.dom(scope(p))) {
      diGraph = diGraph.addEdge(p, v - offset)
    }
    diGraph
  }

  override def init(ps: ProblemState): Outcome = {
    ps.fold(0 until arity)((p, i) => p.shaveDom(scope(i), offset, arity - 1 + offset))
      .andThen { ps =>
        card = scope.map(ps.card).sum
        ps
      }
  }

  /**
    * @return true iff the constraint is satisfied by the given tuple
    */
  override def check(tuple: Array[Int]): Boolean = {
    tuple.forall(i => offset <= i && i < arity + offset) && {
      val cycle = Array.fill(arity + offset)(false)

      var cycleFound = false

      def loop(start: Int): Boolean = {
        cycle(start) || {
          cycle(start) = true
          loop(tuple(start - offset))
        }
      }


      (offset until arity + offset).forall { i =>
        tuple(i - offset) == i || {
          if (cycleFound) {
            cycle(i)
          } else {
            cycleFound = true
            loop(i)
          }
        }
      }
    }
  }

  override def simpleEvaluation: Int = 3

  override protected def advise(problemState: ProblemState, event: Event, pos: Int): Int = card * card
}
