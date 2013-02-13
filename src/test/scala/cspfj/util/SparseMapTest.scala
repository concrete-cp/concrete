package cspfj.util

import org.junit.Test
import org.junit.Assert._

class SparseMapTest {
  @Test
  def test() {
    var map = new SparseMap[String]()
    map += 7 -> "sept"
    map += 6 -> "six"

    println(map)

    assertEquals("sept", map(7))
    assertEquals("six", map(6))
    assertEquals(None, map.get(1))

    println(map.filter { i: Int => (i < 7) })
    println(map)
  }
}