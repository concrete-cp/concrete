package concrete.util

import org.scalatest.Matchers
import org.scalatest.FlatSpec

class IntervalTest extends FlatSpec with Matchers {

  "Intervals" should "divide" in {
    (Interval(1, 19) / 20).allValues shouldBe empty

    (Interval(0, 19) / 20) shouldBe Interval(0, 0)
    (Interval(1, 20) / 20) shouldBe Interval(1, 1)
    (Interval(0, 20) / 20) shouldBe Interval(0, 1)

    (Interval(1, 19) / -20).allValues shouldBe empty
    (Interval(0, 19) / -20) shouldBe Interval(0, 0)
    (Interval(1, 20) / -20) shouldBe Interval(-1, -1)
    (Interval(0, 20) / -20) shouldBe Interval(-1, 0)
  }

  it should "detect intersection" in {
    assert(!(Interval(0, 0) intersects Interval(3, 4)))
    assert(!(Interval(0, 0) intersects Interval(-4, -3)))
    assert(Interval(1, 5) intersects Interval(2, 7))
    assert(Interval(1, 5) intersects Interval(5, 6))
    assert(Interval(1, 5) intersects Interval(2, 4))
    assert(Interval(-5, 3) intersects Interval(1, 4))
  }
}