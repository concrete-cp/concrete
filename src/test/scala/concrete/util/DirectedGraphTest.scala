package concrete.util

import concrete.constraint.semantic.FZSubcircuit
import concrete.{IntDomain, Problem, Variable}
import org.scalatest.{FlatSpec, Matchers}

class DirectedGraphTest extends FlatSpec with Matchers {

  "DirectedGraph" should "buildDomTree" in {
    // Test case from Lengauer & Tarjan 1979

    val graph = new DirectedGraph() //13)

    //R
    .addEdge(0, 1)
    .addEdge(0, 2)
    .addEdge(0, 3)

    //A
    .addEdge(1, 4)

    //B
    .addEdge(2, 1)
    .addEdge(2, 4)
    .addEdge(2, 5)

    //C
    .addEdge(3, 6)
    .addEdge(3, 7)

    //D
    .addEdge(4, 12)

    //E
    .addEdge(5, 8)

    //F
    .addEdge(6, 9)

    //G
    .addEdge(7, 9)
    .addEdge(7, 10)

    //H
    .addEdge(8, 5)
    .addEdge(8, 11)

    //I
    .addEdge(9, 11)

    //J
    .addEdge(10, 9)

    //K
    .addEdge(11, 0)
    .addEdge(11, 9)

    //L
    .addEdge(12, 8)

    val tree = graph.computeDominatorTree(0)

    tree shouldBe Seq(-1, 0, 0, 0, 0, 0, 3, 3, 0, 0, 7, 0, 4)
  }

  it should "compute correct SCCs" in {
    val variables = Array(
      new Variable(s"x1", IntDomain.ofSeq(1, 2, 3)),
      new Variable(s"x2", IntDomain.ofSeq(2, 3, 4, 5)),
      new Variable(s"x3", IntDomain.ofSeq(3, 4)),
      new Variable(s"x4", IntDomain.ofSeq(3, 5)),
      new Variable(s"x5", IntDomain.ofSeq(4)),
    )

    val constraint = new FZSubcircuit(variables)

    val prob = new Problem(variables)
    prob.addConstraint(constraint)

    val ps = prob.initState.toState

    val scc = constraint.buildGraph(ps).findAllSCC()
    scc shouldBe Seq(2, 1, 0, 0, 0)

  }

}
