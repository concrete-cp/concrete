package cspfj.constraint.extension

import org.junit.Test
import org.junit.Assert._

class ListMapTest {

  @Test
  def test() {
    val l = ListMap.empty + (1 -> 2) + (3 -> 4)

    val m = ListMap.empty + (3 -> 4) + (1 -> 2)

    assertEquals(l, m)

    val n = ListMap.empty + (3 -> 4)
    
    assertFalse(m == n)
    assertFalse(l == n)

  }

}