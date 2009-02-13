package cspfj.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Before;
import org.junit.Test;

import cspfj.util.LargeBitVector.IntIterator;

public class BitVectorTest {

	private long[] bitVector;

	@Before
	public void setUp() {
		bitVector = LargeBitVector.newBitVector(125, false);
	}

	@Test
	public void testInitBooleanArray() {
		LargeBitVector.fill(bitVector, 125, true);
		assertEquals(Long.bitCount(bitVector[0]), 64);
		assertTrue(LargeBitVector.get(bitVector, 64));
		assertTrue(LargeBitVector.get(bitVector, 65));
		assertTrue(LargeBitVector.get(bitVector, 124));
		assertFalse(LargeBitVector.get(bitVector, 125));

		LargeBitVector.fill(bitVector, 125, false);
		assertEquals(bitVector[0], 0);
		assertEquals(bitVector[1], 0);

	}

	@Test
	public void testBooleanArraySize() {
		assertEquals(LargeBitVector.nbWords(0), 0);
		assertEquals(LargeBitVector.nbWords(1), 1);
		assertEquals(LargeBitVector.nbWords(64), 1);
		assertEquals(LargeBitVector.nbWords(65), 2);
	}

	@Test
	public void testSet() {
		assertFalse(LargeBitVector.set(bitVector, 100, false));
		assertTrue(LargeBitVector.set(bitVector, 100, true));
		assertFalse(LargeBitVector.set(bitVector, 100, true));
		assertEquals(bitVector[1], 1L << (100 % 64));
		assertEquals(bitVector[0], 0);
		assertTrue(LargeBitVector.set(bitVector, 100, false));
	}

	@Test
	public void testGet() {
		LargeBitVector.set(bitVector, 46, true);
		assertFalse(LargeBitVector.get(bitVector, 0));
		assertFalse(LargeBitVector.get(bitVector, 45));
		assertTrue(LargeBitVector.get(bitVector, 46));
	}

	@Test
	public void testNextSetBit() {
		LargeBitVector.set(bitVector, 46, true);
		LargeBitVector.set(bitVector, 49, true);
		LargeBitVector.set(bitVector, 100, true);
		assertEquals(LargeBitVector.nextSetBit(bitVector, 0), 46);
		assertEquals(LargeBitVector.nextSetBit(bitVector, 46), 46);
		assertEquals(LargeBitVector.nextSetBit(bitVector, 47), 49);
		assertEquals(100, LargeBitVector.nextSetBit(bitVector, 63));
		assertEquals(100, LargeBitVector.nextSetBit(bitVector, 64));
		assertEquals(LargeBitVector.nextSetBit(bitVector, 101), -1);

	}

	@Test
	public void testPrevClearBit() {
		LargeBitVector.fill(bitVector, 125, true);
		LargeBitVector.clear(bitVector, 46);
		LargeBitVector.clear(bitVector, 49);
		LargeBitVector.clear(bitVector, 100);
		assertEquals(46, LargeBitVector.prevClearBit(bitVector, 47));
		assertEquals(-1, LargeBitVector.prevClearBit(bitVector, 46));
		assertEquals(-1, LargeBitVector.prevClearBit(bitVector, 45));
		assertEquals(100, LargeBitVector.prevClearBit(bitVector, 110));

		assertEquals(49, LargeBitVector.prevClearBit(bitVector, 64));
		assertEquals(49, LargeBitVector.prevClearBit(bitVector, 63));

		LargeBitVector.clear(bitVector, 64);
		assertEquals(64, LargeBitVector.prevClearBit(bitVector, 65));
		assertEquals(49, LargeBitVector.prevClearBit(bitVector, 64));

		LargeBitVector.set(bitVector, 64);
		LargeBitVector.clear(bitVector, 63);
		assertEquals(63, LargeBitVector.prevClearBit(bitVector, 65));
		assertEquals(49, LargeBitVector.prevClearBit(bitVector, 63));

	}

	@Test
	public void testPrevSetBit() {
		LargeBitVector.set(bitVector, 46);
		LargeBitVector.set(bitVector, 49);
		LargeBitVector.set(bitVector, 100);
		assertEquals(46, LargeBitVector.prevSetBit(bitVector, 47));
		assertEquals(-1, LargeBitVector.prevSetBit(bitVector, 46));
		assertEquals(-1, LargeBitVector.prevSetBit(bitVector, 45));
		assertEquals(100, LargeBitVector.prevSetBit(bitVector, 110));

		assertEquals(49, LargeBitVector.prevSetBit(bitVector, 64));
		assertEquals(49, LargeBitVector.prevSetBit(bitVector, 63));

		LargeBitVector.set(bitVector, 64);
		assertEquals(64, LargeBitVector.prevSetBit(bitVector, 65));
		assertEquals(49, LargeBitVector.prevSetBit(bitVector, 64));

		LargeBitVector.clear(bitVector, 64);
		LargeBitVector.set(bitVector, 63);
		assertEquals(63, LargeBitVector.prevSetBit(bitVector, 65));
		assertEquals(49, LargeBitVector.prevSetBit(bitVector, 63));

	}

	@Test
	public void testToStringIntArray() {
		LargeBitVector.set(bitVector, 46, true);
		LargeBitVector.set(bitVector, 49, true);
		LargeBitVector.set(bitVector, 100, true);
		assertEquals("{46, 49, 100}", LargeBitVector.toString(bitVector));
	}

	@Test
	public void testWord() {
		assertEquals(0, LargeBitVector.word(0));
		assertEquals(0, LargeBitVector.word(63));
		assertEquals(1, LargeBitVector.word(64));
	}

	@Test
	public void testIterator() {
		LargeBitVector.set(bitVector, 46);
		LargeBitVector.set(bitVector, 49);
		LargeBitVector.set(bitVector, 100);
		IntIterator it = LargeBitVector.iterator(bitVector);
		assertEquals(46, it.next());
		assertEquals(49, it.next());
		assertEquals(100, it.next());
		assertFalse(it.hasNext());
		
		
	}
}
