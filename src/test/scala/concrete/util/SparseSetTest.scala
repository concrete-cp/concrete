package concrete.util

import org.junit.Test
import org.junit.Assert._

class SparseSetTest {
  @Test
  def test() {
    val map = new SparseSet(10) + 7

    val map2 = map + 6

    println(map2)

    assertTrue(map2(7))
    assertTrue(map2(6))
    assertFalse(map2(1))

    assertFalse(map(6))

  }
}