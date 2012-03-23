package cspfj.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Before;
import org.junit.Test;

public class SmallBitVectorTest {

    private SmallBitVector bitVector;

    @Before
    public void setUp() {
        bitVector = new SmallBitVector(50);
    }

    @Test
    public void testInitBooleanArray() {
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
    public void testSet() {
        assertFalse(bitVector.set(30, false));
        assertTrue(bitVector.set(30, true));
        assertFalse(bitVector.set(30, true));
    }

    @Test
    public void testGet() {
        bitVector.set(46);
        assertFalse(bitVector.get(0));
        assertFalse(bitVector.get(45));
        assertTrue(bitVector.get(46));
    }

    @Test
    public void testNextSetBit() {
        bitVector.set(46);
        assertEquals(46, bitVector.nextSetBit(0));
        assertEquals(46, bitVector.nextSetBit(46));
        assertEquals(-1, bitVector.nextSetBit(47));
        bitVector.set(63);
        assertEquals(63, bitVector.nextSetBit(47));

    }

    @Test
    public void testPrevClearBit() {
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
    public void testPrevSetBit() {
        bitVector.set(46);
        bitVector.set(49);

        assertEquals(46, bitVector.prevSetBit(47));
        assertEquals(-1, bitVector.prevSetBit(46));
        assertEquals(-1, bitVector.prevSetBit(45));

        assertEquals(49, bitVector.prevSetBit(63));
        assertEquals(49, bitVector.prevSetBit(64));
        assertEquals(49, bitVector.prevSetBit(65));

        bitVector.set(63);
        assertEquals(63, bitVector.prevSetBit(65));
        assertEquals(49, bitVector.prevSetBit(63));

    }

    @Test
    public void testToStringIntArray() {
        bitVector.set(46);
        bitVector.set(49);
        assertEquals("{46, 49}", bitVector.toString());
    }

    @Test
    public void testWord() {
        assertEquals(0, LargeBitVector.word(0));
        assertEquals(0, LargeBitVector.word(63));
        assertEquals(1, LargeBitVector.word(64));
    }

    @Test
    public void testClearFrom() {
        bitVector.set(46);
        bitVector.set(49);
        bitVector.set(60);
        assertTrue(bitVector.clearFrom(47));
        assertEquals(1, bitVector.cardinality());
        assertTrue(bitVector.get(46));
        assertFalse(bitVector.get(49));
        assertFalse(bitVector.get(60));
    }

    @Test
    public void testSetFrom() {
        assertTrue(bitVector.setFrom(30));
        assertEquals(bitVector.toString(), 20, bitVector.cardinality());
        for (int i = 0; i < 30; i++) {
            assertFalse(bitVector.get(i));
        }
        for (int i = 30; i < 50; i++) {
            assertTrue(Integer.toString(i), bitVector.get(i));
        }
        for (int i = 50; i < 100; i++) {
            assertFalse(Integer.toString(i), bitVector.get(i));
        }
    }

    @Test
    public void testClearTo() {
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
    public void testSubset() {
        bitVector.set(46);
        bitVector.set(49);
        bitVector.set(60);

        BitVector bv2 = BitVector.newBitVector(64);
        bv2.set(46);

        assertFalse(bitVector.subsetOf(bv2));
        assertTrue(bv2.subsetOf(bitVector));
    }

    @Test
    public void testEquals() {
        bitVector.set(46);
        BitVector lbv = BitVector.newBitVector(128);
        lbv.set(46);
        assertEquals(bitVector, lbv);
        assertEquals(lbv, bitVector);
        lbv.set(100);
        assertFalse(bitVector.equals(lbv));
        assertFalse(lbv.equals(bitVector));
    }
    // @Test
    // public void testIterator() {
    // bitVector.set( 46);
    // bitVector.set( 49);
    // bitVector.set( 100);
    // IntIterator it = bitVector.i
    // assertEquals(46, it.next());
    // assertEquals(49, it.next());
    // assertEquals(100, it.next());
    // assertFalse(it.hasNext());
    //
    // }
}
