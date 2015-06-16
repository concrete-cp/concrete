package concrete

import org.scalatest.FlatSpec
import org.scalatest.Matchers

final class IntDomainTest extends FlatSpec with Matchers {

  "IntDomain" should "find next/prev" in {
    val b = IntDomain.ofSeq(1, 2, 7, 8)
    b.prev(3) shouldBe 2
    b.prev(2) shouldBe 1
    b.next(2) shouldBe 7
    b.prev(9) shouldBe 8
    a[NoSuchElementException] should be thrownBy b.next(9)
    a[NoSuchElementException] should be thrownBy b.prev(1)
    a[NoSuchElementException] should be thrownBy b.prev(0)
  }

  it should "enumerate" in {
    IntDomain.ofSeq(1, 2, 7, 8) should contain theSameElementsAs Seq(1, 2, 7, 8)
  }

  it should "detect presence" in {
    val domain = IntDomain.ofSeq(0, 1)
    assert(domain.present(0))

    val d2 = domain.remove(0);
    assert(!d2.present(0));

    assert(domain.present(0));
  }

  it should "split" in {
    val domain = IntDomain.ofSeq(3, 4, 5, 7)
    domain.removeTo(4).head shouldBe 5
    domain.removeFrom(6).last shouldBe 5
    domain.removeFrom(0) shouldBe empty
    domain.removeFrom(3) shouldBe empty
    domain.removeTo(7) shouldBe empty
    domain.removeTo(8) shouldBe empty
    domain.removeFrom(-1) shouldBe empty
    domain.removeUntil(7) should contain theSameElementsAs Seq(7)
    domain.removeUntil(400) shouldBe empty
    domain.removeFrom(400) shouldBe domain
  }
}