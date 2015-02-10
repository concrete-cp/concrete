package concrete

import org.scalatest.Matchers
import org.scalatest.FlatSpec

class IntervalDomainTest extends FlatSpec with Matchers {

  "IntervalDomain" should "test" in {
    val dom = IntDomain(10 to 20)

    dom.head shouldBe 10
    dom.last shouldBe 20
    dom shouldBe (10 to 20)
    dom.removeFrom(14).last shouldBe 13
    dom.removeTo(13).head shouldBe 14
  }
}