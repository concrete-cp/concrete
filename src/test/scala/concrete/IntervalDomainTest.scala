package concrete

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class IntervalDomainTest extends AnyFlatSpec with Matchers {

  "IntervalDomain" should "test" in {
    val dom = IntDomain(10 to 20)

    dom.head shouldBe 10
    dom.last shouldBe 20
    dom.view should contain theSameElementsAs (10 to 20)
    dom.removeFrom(14).last shouldBe 13
    dom.removeTo(13).head shouldBe 14
  }

  it should "remove values" in {
    val dom = IntDomain.ofInterval(0, 224) - 223
    dom should not contain 223

    val d2 = dom.removeAfter(224)
    assert(d2 subsetOf dom)
    assert(d2.head == 0)
    assert(dom.head == 0)
  }

  it should "not remove inexistent values" in {
    val dom = IntDomain.ofInterval(4, 6) - 4
    dom - 4 shouldBe dom
  }
}