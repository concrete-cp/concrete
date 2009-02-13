package cspfj.constraint;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;

import org.junit.Before;
import org.junit.Test;

import cspfj.constraint.extension.TupleSet;

public class TupleSetTest {

	TupleSet ts;

	@Before
	public void setUp() throws Exception {
		ts = new TupleSet(1);
		ts.add(new int[] { 0, 1 });
		ts.add(new int[] { 0, 2 });
		ts.add(new int[] { 1, 1 });
		ts.add(new int[] { 2, 4 });
		ts.removeTuple(new int[] { 1, 1 });
	}

	@Test
	public void testSize() {
		assertEquals(ts.size(), 3);
	}

	@Test
	public void testContainsTuple() {
		assertTrue(ts.containsTuple(new int[] { 2, 4 }));
		assertFalse(ts.containsTuple(new int[] { 1, 1 }));
		assertFalse(ts.containsTuple(new int[] { 5, 5 }));
	}

	@Test
	public void testRemoveTuple() {
		ts.removeTuple(new int[] { 2, 4 });
		assertFalse(ts.containsTuple(new int[] { 2, 4 }));
		ts.removeTuple(new int[] { 5, 5 });
	}

	@Test
	public void testAddIntArray() {
		ts.add(new int[] { 10, 10 });
		assertTrue(ts.containsTuple(new int[] { 10, 10 }));
	}

	@Test
	public void testIterator() {
		int i = 0;
		for (int[] t : ts) {
			System.out.println(Arrays.toString(t));
			i++;
		}
		assertEquals(i, ts.size());
	}

}
