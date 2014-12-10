package concrete.constraint.semantic
import concrete.IntDomain
import concrete.Variable
import concrete.constraint.AdviseCount
import org.scalatest.Matchers
import concrete.Revised
import org.scalatest.FlatSpec

final class EqTest extends FlatSpec with Matchers {

  "Eq" should "filter" in {
    val v0 = new Variable("v0", IntDomain(1, 2, 3))
    val v1 = new Variable("v1", IntDomain(3, 4, 5))
    val c = new EqAC(v0, v1)
    c.register(new AdviseCount())
    val d = IndexedSeq(v0.initDomain, v1.initDomain)
    c.adviseAll(d)
    val Revised(mod, _, _) = c.revise(d, Unit)

    mod(0) shouldBe Seq(3)
    mod(1) shouldBe Seq(3)
  }
}