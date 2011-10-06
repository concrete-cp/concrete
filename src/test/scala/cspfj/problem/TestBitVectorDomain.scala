package cspfj.problem
import org.junit.Test
import org.junit.Assert._

final class TestBitVectorDomain {

  @Test
  def testGreatest() {
    val b = new BitVectorDomain(1, 2, 7, 8)
    assertEquals(1, b.closestLeq(3));
    assertEquals(2, b.closestGeq(3));
    assertEquals(3, b.closestLeq(9));
    assertEquals(-1, b.closestGeq(9))
    assertEquals(0, b.closestLeq(1))
    assertEquals(-1, b.closestLeq(0))
  }
}