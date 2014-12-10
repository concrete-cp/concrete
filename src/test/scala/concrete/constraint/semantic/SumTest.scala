package concrete.constraint.semantic;

import concrete.constraint.Constraint
import concrete.IntDomain
import concrete.Variable
import concrete.constraint.AdviseCount
import concrete.constraint.semantic.SumMode._
import org.scalatest.Matchers
import concrete.Revised
import org.scalatest.FlatSpec

final class SumTest extends FlatSpec with Matchers {

  "Sum" should "filter <= with negative coefficients" in {
    val x = new Variable("x", IntDomain(0, 1))
    val y = new Variable("y", IntDomain(0, 1))
    val c = new Sum(-19, Array(-20, -18), Array(x, y), SumLE)
    val domains = IndexedSeq(x.initDomain, y.initDomain)
    c.adviseAll(domains)
    val Revised(mod, _, _) = c.revise(domains, Unit)
    mod(0) shouldBe Seq(1)
    mod(1) shouldBe Seq(0, 1)

  }

  val v0 = new Variable("v0", IntDomain(1 to 4))
  val v1 = new Variable("v1", IntDomain(0 to 4))
  val b = new Variable("b", IntDomain(1))

  val domains = IndexedSeq(b.initDomain, v0.initDomain, v1.initDomain)

  it should "filter =" in {
    val c = new Sum(0, Array(4, -1, -1), Array(b, v0, v1), SumEQ);
    c.adviseAll(domains)
    val Revised(mod, _, _) = c.revise(domains, Unit)

    mod(1) shouldBe Seq(1, 2, 3, 4)
    mod(2) shouldBe Seq(0, 1, 2, 3)
  }

  it should "filter <= with positive and negative coefficients" in {
    val c = new Sum(0, Array(4, -1, -1), Array(b, v0, v1), SumLE);
    c.adviseAll(domains)
    val Revised(mod, _, _) = c.revise(domains, Unit)

    mod(1) shouldBe Seq(1, 2, 3, 4)
    mod(2) shouldBe (0 to 4)
  }

  it should "filter <= with positive coefficients" in {
    val c = new Sum(3, Array(1, 1), Array(v0, v1), SumLE)
    val dom = IndexedSeq(v0.initDomain, v1.initDomain)
    c.adviseAll(dom)
    val Revised(mod, _, _) = c.revise(dom, Unit)
    mod(0) shouldBe (1 to 3)
    mod(1) shouldBe (0 to 2)
  }

  it should "filter <= with other negative coefficients and domains" in {

    val v2 = new Variable("v2", IntDomain(0 to 1))
    val c = new Sum(-3, Array(-1, -1, -6), Array(
      new Variable("v0", IntDomain(0 to 1)),
      new Variable("v1", IntDomain(1 to 5)),
      v2), SumLE)

    val dom = c.scope.map(_.initDomain)
    c.adviseAll(dom)
    val Revised(mod, _, _) = c.revise(dom, Unit)
    mod(2) shouldBe (0 to 1)

  }

  it should "filter = with particular parameters" in {
    val q43 = new Variable("q43", IntDomain(0 to 3))
    val q44 = new Variable("q44", IntDomain(1 to 1))
    val q45 = new Variable("q45", IntDomain(1 to 2))
    val q46 = new Variable("q46", IntDomain(0 to 0))
    val q47 = new Variable("q47", IntDomain(0 to 0))
    val q48 = new Variable("q48", IntDomain(0 to 0))

    val c = new Sum(-6, Array(-1, -2, -3, -4, -5, -6), Array(q43, q44, q45, q46, q47, q48), SumEQ)
    val domains = c.scope.map(_.initDomain)
    c.adviseAll(domains)
    val Revised(mod, _, _) = c.revise(domains, Unit)
    mod(0) shouldBe Seq(1)

    // -1.Q[43] [0, 3] + -2.Q[44] [1] + -3.Q[45] [1, 2] + -4.Q[46] [0] + -5.Q[47] [0] + -6.Q[48] [0] eq -6 ()
  }

}
