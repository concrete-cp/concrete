package cspfj.problem
import org.junit.Assert.assertEquals
import org.junit.Test

final class TestBitVectorDomain {

  @Test
  def testGreatest() {
    val b = new BitVectorDomain(1, 2, 7, 8)
    assertEquals(1, b.closestLt(3));
    assertEquals(2, b.closestGt(3));
    assertEquals(3, b.closestLt(9));
    assertEquals(-1, b.closestGt(9))
    assertEquals(-1, b.closestLt(1))
    assertEquals(-1, b.closestLt(0))
  }
}