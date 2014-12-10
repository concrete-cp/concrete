package concrete.constraint.semantic;

import concrete.IntDomain
import concrete.Variable
import org.scalatest.Matchers
import concrete.Revised
import org.scalatest.FlatSpec

final class GtTest extends FlatSpec with Matchers {

  val v1: Variable = new Variable("v1", IntDomain(1 to 4));
  val v2: Variable = new Variable("v2", IntDomain(3 to 5));

  val domains = IndexedSeq(v1.initDomain, v2.initDomain)

  "Gt" should "strictly filter" in {
    val c = new Gt(v1, v2, true);
    val Revised(mod, _, _) = c.revise(domains, Unit);

    mod(0) shouldBe Seq(4)
    mod(1) shouldBe Seq(3)
  }

  "Gt" should "not strictly filter" in {
    val c = new Gt(v1, v2, false);
    val Revised(mod, _, _) = c.revise(domains, Unit);

    mod(0) shouldBe Seq(3, 4)
    mod(1) shouldBe Seq(3, 4)

  }

}
