package cspfj.problem;

import cspfj.util.BitVector
import org.junit.Assert.{ assertTrue, assertFalse, assertEquals, assertArrayEquals }
import org.junit.{ Test, Before }

final class BooleanDomainTest {

  private var domain: Domain = null;

  @Before
  def setup() {
    domain = new BooleanDomain
  }

  @Test
  def testSize() {
    assertEquals(2, domain.size);
    domain.setLevel(1);
    domain.remove(0);
    assertEquals(1, domain.size);
    domain.restoreLevel(0);
    assertEquals(2, domain.size);

  }

  @Test
  def testIndex() {
    assertEquals(1, domain.index(1));
  }

  @Test
  def testPresent() {
    assertTrue(domain.present(0));
    domain.setLevel(1);
    domain.remove(0);
    assertFalse(domain.present(0));
    domain.restoreLevel(0);
    assertTrue(domain.present(0));
  }

  @Test
  def testAssign() {
    domain.setSingle(0);
    assertFalse(domain.present(1));
    assertTrue(domain.present(0));
  }

  @Test
  def testUnassign() {
    domain.setLevel(1);
    domain.setSingle(0);
    domain.restoreLevel(0);
    assertTrue(domain.present(1));
    assertTrue(domain.present(0));
  }

  @Test
  def testRestore() {
    domain.remove(0);
    domain.setLevel(1);
    domain.remove(1);
    assertFalse(domain.present(1));

    domain.restoreLevel(0);
    assertTrue(domain.present(1));
    assertFalse(domain.present(0));

    domain.setLevel(3);
    domain.remove(1);
    domain.restoreLevel(0);
    assertTrue(domain.present(1));
    assertFalse(domain.present(0));
  }

  @Test
  def testFirst() {
    assertEquals(0, domain.firstIndex);
    domain.remove(0);
    assertEquals(1, domain.firstIndex);
  }

  @Test
  def testLast() {
    assertEquals(1, domain.lastIndex);
    domain.remove(1);
    assertEquals(0, domain.lastIndex);
  }

  @Test
  def testCurrentIndexes() {
    assertArrayEquals(Array(0, 1), domain.currentIndexes);
    domain.remove(1);
    assertArrayEquals(Array(0), domain.currentIndexes);
  }

  @Test
  def testNext() {
    assertEquals(1, domain.next(0))
    assertEquals(-1, domain.next(1))
    domain.remove(1)
    assertEquals(-1, domain.next(0))
  }

  @Test
  def testPrev() {
    assertEquals(0, domain.prev(1));
    assertEquals(-1, domain.prev(0));
    domain.remove(0);
    assertEquals(-1, domain.prev(1));

  }

  @Test
  def testLastAbsent() {
    assertEquals(-1, domain.lastAbsent);
    domain.remove(0);
    assertEquals(0, domain.lastAbsent);
    domain.remove(1);
    assertEquals(1, domain.lastAbsent);
  }

  @Test
  def testPrevAbsent() {
    assertEquals(-1, domain.prevAbsent(1));
    domain.remove(0);
    assertEquals(0, domain.prevAbsent(1));
    assertEquals(-1, domain.prevAbsent(0));
  }

  @Test
  def testBitVector() {
    assertEquals(BitVector.newBitVector(2, true), domain.getBitVector);
  }

  // @Test
  // public void testClone() throws CloneNotSupportedException {
  // final domain clone = domain.clone();
  // assertNotSame(clone, domain);
  // assertNotSame(clone.getBitDomain(), domain.getBitDomain());
  // }

}
