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
}