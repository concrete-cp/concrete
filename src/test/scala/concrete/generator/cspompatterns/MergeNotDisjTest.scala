package concrete.generator.cspompatterns

import cspom.CSPOM
import cspom.variable.IntVariable
import org.scalatest.{FlatSpec, Matchers}
import CSPOM._
import concrete.CSPOMDriver._
import concrete.Solver

class MergeNotDisjTest extends FlatSpec with Matchers {
  "MergeNotDisj" should "work with non-clause constraints" in {
    val prob = CSPOM { implicit problem =>
      val a = IntVariable(1 to 10) as "a"
      val b = IntVariable(1 to 10) as "b"

      val r1 = (a + b < 8)
      val r2 = !(a + b < 9)

      ctr(r1 | r2)
    }

    val solver = Solver(prob).get
//    println(solver.concreteProblem)
//    for (s <- solver) println(s)
    solver.size shouldBe 93
  }
}
