package concrete

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

final class TestIntDomain {

  @Test
  def testGreatest() {
    val b = IntDomain(1, 2, 7, 8)
    assertEquals(1, b.closestLt(3));
    assertEquals(2, b.closestGt(3));
    assertEquals(3, b.closestLt(9));
    assertEquals(-1, b.closestGt(9))
    assertEquals(-1, b.closestLt(1))
    assertEquals(-1, b.closestLt(0))
  }

  @Test
  def testPresent() {
    val domain = IntDomain(0, 1)
    assertTrue(domain.present(0));
    domain.setLevel(1);
    domain.remove(0);
    assertFalse(domain.present(0));
    domain.restoreLevel(0);
    assertTrue(domain.present(0));
  }

  @Test
  def testIntervals() {
    val domain = IntDomain(3, 4, 5, 7)
    domain.removeToVal(4)
    assertEquals(domain.toString, 5, domain.firstValue)

    domain.removeFromVal(6)
    assertEquals(domain.toString, 5, domain.lastValue)
  }
}