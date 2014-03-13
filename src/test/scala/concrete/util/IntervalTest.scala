package concrete.util

import org.junit.Test
import org.junit.Assert._

class IntervalTest {
  @Test
  def testDiv() {
    assertTrue((Interval(1, 19) / 20).allValues.isEmpty)
    assertEquals(Interval(0, 0), Interval(0, 19) / 20)
    assertEquals(Interval(1, 1), Interval(1, 20) / 20)
    assertEquals(Interval(0, 1), Interval(0, 20) / 20)

    assertTrue((Interval(1, 19) / -20).allValues.isEmpty)
    assertEquals(Interval(0, 0), Interval(0, 19) / -20)
    assertEquals(Interval(-1, -1), Interval(1, 20) / -20)
    assertEquals(Interval(-1, 0), Interval(0, 20) / -20)
  }
}