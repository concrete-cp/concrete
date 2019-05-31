package concrete

import org.scalatest.FlatSpec
import org.scalatest.Matchers

final class BooleanDomainTest extends FlatSpec with Matchers {

  private val domain = BooleanDomain()

  "BooleanDomain" should "compute size" in {
    domain should have size 2
    val d2 = domain - 0
    d2 should have size 1
    domain should have size 2
  }

  it should "detect presence" in {
    assert(domain.contains(0))
    val d2 = domain - 0
    assert(!d2.contains(0))
    assert(domain.contains(0))
  }

  it should "be persistent" in {
    val d2 = domain - 1
    assert(!d2.contains(1))
    assert(domain.contains(1))
    domain - 1
    assert(domain.contains(1))
  }

  it should "find first value" in {
    domain.head shouldBe 0
    domain.head shouldBe 0
    val d2 = domain - 0
    d2.head shouldBe 1
  }

  it should "list values" in {
    domain.view should contain theSameElementsAs Seq(0, 1)
    domain - 1 should contain theSameElementsAs Seq(0)
  }

  it should "find next value" in {
    domain.next(0) shouldBe 1
    a[NoSuchElementException] should be thrownBy domain.next(1)
    val d2 = domain.-(1)
    a[NoSuchElementException] should be thrownBy d2.next(0)
  }

  it should "find prev value" in {
    domain.prev(1) shouldBe 0
    a[NoSuchElementException] should be thrownBy domain.prev(0)
    val d2 = domain.-(0)
    a[NoSuchElementException] should be thrownBy d2.prev(1)
  }

}
