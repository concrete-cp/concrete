package concrete.constraint.semantic

import bitvectors.BitVector
import concrete._
import concrete.constraint.{Constraint, StatefulConstraint}
import concrete.util.DirectedGraph

/**
  * Allows at most one subcircuit
  *
  * @param scope
  */
final class FZSubcircuit(scope: Array[Variable], offset: Int = 1)
  extends Constraint(scope)
    with StatefulConstraint[DirectedGraph] {
  //  private val diGraph = new DirectedGraph(arity)
  var eval: Int = _

  override def toString(ps: ProblemState) = super.toString(ps) + s", offset=${offset}"

  override def revise(ps: ProblemState, modified: BitVector): Outcome = {
    val directedGraph = updatedGraph(ps, modified)

    filterSCC(directedGraph, ps)
  }

  private def updatedGraph(ps: ProblemState, modified: BitVector) = {
    var graph = ps(this)
    for (m <- modified) {
      val dom = ps.dom(scope(m))
      graph = graph.filterSucc(m, i => dom.contains(i + offset))
    }
    graph
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
    //logger.info(s"SCC: ${scc.toSeq}")
    val mandatoryCycles = findMandatoryCycles(directedGraph, scc)
    //logger.info(s"Mandatory cycles: $mandatoryCycles")
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
        .andThen { ps =>
          val updatedGraph = Iterator.range(0, arity)
            .filter(p => scc(p) != mc)
            .foldLeft(directedGraph)((graph, p) =>
              graph.setSucc(p,  Set(p))
            )
          ps.updateState(this, updatedGraph)
        }
    } else {
      val culprit = for {
        i <- Iterator.range(0, arity)
        if mandatoryCycles.contains(scc(i))
      } yield {
        scope(i)
      }
      Contradiction(culprit.toSeq)
    }
  }

  def findMandatoryCycles(diGraph: DirectedGraph, scc: Array[Int]): Set[Int] = {
    Iterator.range(0, arity)
      .filterNot(i => diGraph.succ(i).contains(i))
      .map(scc(_))
      .toSet
  }

  override def init(ps: ProblemState): Outcome = {
    ps.fold(0 until arity)((p, i) => p.shaveDom(scope(i), offset, arity - 1 + offset))
      .andThen { ps =>
        eval = scope.map(ps.card).fold(arity)(Math.addExact)
        val digraph = buildGraph(ps)
        ps.updateState(this, digraph)
      }
  }

  def buildGraph(ps: ProblemState): DirectedGraph = {
    var diGraph = new DirectedGraph() //.clear()
    for (p <- 0 until arity; v <- ps.dom(scope(p))) {
      diGraph = diGraph.addEdge(p, v - offset)
    }
    diGraph
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

  override protected def advise(problemState: ProblemState, event: Event, pos: Int): Int =
    eval
}
