package concrete.util

import org.scalatest.{FlatSpec, Matchers, OptionValues}

class IntervalTest extends FlatSpec with Matchers with OptionValues {

  "Intervals" should "divide" in {
    (Interval(1, 19) / 20) shouldBe empty

    (Interval(0, 19) / 20).value shouldBe Interval(0, 0)
    (Interval(1, 20) / 20).value shouldBe Interval(1, 1)
    (Interval(0, 20) / 20).value shouldBe Interval(0, 1)

    (Interval(1, 19) / -20) shouldBe empty
    (Interval(0, 19) / -20).value shouldBe Interval(0, 0)
    (Interval(1, 20) / -20).value shouldBe Interval(-1, -1)
    (Interval(0, 20) / -20).value shouldBe Interval(-1, 0)

    (Interval(-68, -12) / Interval(35, 46)).value shouldBe Interval(-1, -1)


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