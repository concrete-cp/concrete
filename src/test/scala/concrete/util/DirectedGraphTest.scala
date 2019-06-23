package concrete.util

import org.scalatest.{FlatSpec, Matchers}

class DirectedGraphTest extends FlatSpec with Matchers {

  "DirectedGraph" should "buildDomTree" in {
    // Test case from Lengauer & Tarjan 1979

    val graph = new DirectedGraph(13)

    //R
    graph.addArc(0, 1)
    graph.addArc(0, 2)
    graph.addArc(0, 3)

    //A
    graph.addArc(1, 4)

    //B
    graph.addArc(2, 1)
    graph.addArc(2, 4)
    graph.addArc(2, 5)

    //C
    graph.addArc(3, 6)
    graph.addArc(3, 7)

    //D
    graph.addArc(4, 12)

    //E
    graph.addArc(5, 8)

    //F
    graph.addArc(6, 9)

    //G
    graph.addArc(7, 9)
    graph.addArc(7, 10)

    //H
    graph.addArc(8, 5)
    graph.addArc(8, 11)

    //I
    graph.addArc(9, 11)

    //J
    graph.addArc(10, 9)

    //K
    graph.addArc(11, 0)
    graph.addArc(11, 9)

    //L
    graph.addArc(12, 8)

    val tree = graph.computeDominatorTree(0)

    tree shouldBe Seq(-1, 0, 0, 0, 0, 0, 3, 3, 0, 0, 7, 0, 4)
  }


}
