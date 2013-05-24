package concrete.util;

import org.junit.Assert.assertEquals;
import org.junit.Assert.assertFalse;
import org.junit.Assert.assertTrue;

import org.junit.Before;
import org.junit.Test;

class SmallBitVectorTest {

  private var bitVector: SmallBitVector = null;

  @Before
  def setUp() {
    bitVector = new SmallBitVector(50);
  }

  @Test
  def testInitBooleanArray() {
    bitVector.fill(true);
    // assertEquals(bitVector.bitCount(), 64);
    assertTrue(bitVector.get(49));
    assertTrue(bitVector.get(32));
    assertFalse(bitVector.get(50));
    assertEquals(50, bitVector.cardinality());

    bitVector.fill(false);
    assertEquals(-1, bitVector.nextSetBit(0));
  }

  @Test
  def testSet() {
    assertFalse(bitVector.set(30, false));
    assertTrue(bitVector.set(30, true));
    assertFalse(bitVector.set(30, true));
  }

  @Test
  def testGet() {
    bitVector.set(46);
    assertFalse(bitVector.get(0));
    assertFalse(bitVector.get(45));
    assertTrue(bitVector.get(46));
  }

  @Test
  def testNextSetBit() {
    bitVector.set(46);
    assertEquals(46, bitVector.nextSetBit(0));
    assertEquals(46, bitVector.nextSetBit(46));
    assertEquals(-1, bitVector.nextSetBit(47));
    bitVector.set(49);
    assertEquals(49, bitVector.nextSetBit(47));

  }

  @Test
  def testPrevClearBit() {
    bitVector.fill(true);
    bitVector.clear(46);
    bitVector.clear(48);
    assertEquals(46, bitVector.prevClearBit(47));
    assertEquals(-1, bitVector.prevClearBit(46));
    assertEquals(-1, bitVector.prevClearBit(45));

    assertEquals(48, bitVector.prevClearBit(50));
    assertEquals(48, bitVector.prevClearBit(49));

  }

  @Test
  def testPrevSetBit() {
    bitVector.set(26);
    bitVector.set(29);

    assertEquals(26, bitVector.prevSetBit(27));
    assertEquals(-1, bitVector.prevSetBit(26));
    assertEquals(-1, bitVector.prevSetBit(25));

    assertEquals(29, bitVector.prevSetBit(51));
    assertEquals(29, bitVector.prevSetBit(50));
    assertEquals(29, bitVector.prevSetBit(49));

    bitVector.set(49);
    assertEquals(49, bitVector.prevSetBit(65));
    assertEquals(49, bitVector.prevSetBit(63));

  }

  @Test
  def testToStringIntArray() {
    bitVector.set(46);
    bitVector.set(49);
    assertEquals("{46, 49}", bitVector.toString());
  }

  @Test
  def testWord() {
    assertEquals(0, LargeBitVector.word(0));
    assertEquals(0, LargeBitVector.word(63));
    assertEquals(1, LargeBitVector.word(64));
  }

  @Test
  def testClearFrom() {
    bitVector.set(26);
    bitVector.set(29);
    bitVector.set(30);
    assertTrue(bitVector.clearFrom(27));
    assertEquals(1, bitVector.cardinality());
    assertTrue(bitVector.get(26));
    assertFalse(bitVector.get(29));
    assertFalse(bitVector.get(30));
  }

  @Test
  def testSetFrom() {
    assertTrue(bitVector.setFrom(30));
    assertEquals(bitVector.toString(), 20, bitVector.cardinality());
    for (i <- 0 until 30) {
      assertFalse(bitVector.get(i));
    }
    for (i <- 30 until 50) {
      assertTrue(Integer.toString(i), bitVector.get(i));
    }
    for (i <- 50 until 100) {
      assertFalse(Integer.toString(i), bitVector.get(i));
    }
  }

  @Test
  def testClearTo() {
    bitVector.set(26);
    bitVector.set(29);
    bitVector.set(40);
    assertTrue(bitVector.clearTo(29));
    assertEquals(2, bitVector.cardinality());
    assertFalse(bitVector.get(26));
    assertTrue(bitVector.get(29));
    assertTrue(bitVector.get(40));
    assertFalse(bitVector.clearTo(0));
    assertFalse(bitVector.get(26));
    assertTrue(bitVector.get(29));
    assertTrue(bitVector.get(40));
  }

  @Test
  def testSubset() {
    bitVector.set(26);
    bitVector.set(29);
    bitVector.set(30);

    val bv2 = BitVector.newBitVector(50);
    bv2.set(26);

    assertFalse(bitVector.subsetOf(bv2));
    assertTrue(bv2.subsetOf(bitVector));
  }

  @Test
  def testEquals() {
    bitVector.set(46);
    val lbv = BitVector.newBitVector(128);
    lbv.set(46);
    assertEquals(bitVector, lbv);
    assertEquals(lbv, bitVector);
    lbv.set(100);
    assertFalse(bitVector.equals(lbv));
    assertFalse(lbv.equals(bitVector));
  }

}
