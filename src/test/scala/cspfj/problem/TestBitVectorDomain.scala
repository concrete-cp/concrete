package cspfj.problem
import org.junit.Test
import org.junit.Assert._

final class TestBitVectorDomain {

  @Test
  def testGreatest() {
    val b = new BitVectorDomain(1, 2, 7, 8)
    assertEquals(2, b.greatest(3));
    assertEquals(1, b.lowest(3));
    assertEquals(-1, b.greatest(9));
  }
}