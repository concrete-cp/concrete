package concrete

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Before
import org.junit.Test
import org.scalatest.Matchers
import org.scalatest.FlatSpec

final class BooleanDomainTest extends FlatSpec with Matchers {

  private val domain = BooleanDomain()

  "BooleanDomain" should "compute size" in {
    domain should have size 2
    val d2 = domain.remove(0)
    d2 should have size 1
    domain should have size 2
  }

  it should "detect presence" in {
    assert(domain.present(0))
    val d2 = domain.remove(0)
    assert(!d2.present(0))
    assert(domain.present(0))
  }


  it should "be persistent" in {
    val d2 = domain.remove(1)
    assert(!d2.present(1))
    assert(domain.present(1))
    val d3 = domain.remove(1)
    assert(domain.present(1))
  }

  it should "find first value" in {
    domain.head shouldBe 0
    domain.head shouldBe 0
    val d2 = domain.remove(0)
    d2.head shouldBe 1
  }

  it should "list values" in {
    domain shouldBe Seq(0, 1)
    val d2 = domain.remove(1)
    d2 shouldBe Seq(0)
  }

  it should "find next value" in {
    domain.next(0) shouldBe 1
    a[NoSuchElementException] should be thrownBy domain.next(1)
    val d2 = domain.remove(1)
    a[NoSuchElementException] should be thrownBy d2.next(0)
  }

  it should "find prev value" in {
    domain.prev(1) shouldBe 0
    a[NoSuchElementException] should be thrownBy domain.prev(0)
    val d2 = domain.remove(0)
    a[NoSuchElementException] should be thrownBy d2.prev(1)
  }

}
