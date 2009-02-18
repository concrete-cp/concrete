package cspfj.constraint.extension;

import static org.junit.Assert.*;
import static org.junit.Assert.fail;

import java.util.Iterator;

import org.junit.Before;
import org.junit.Test;

import cspfj.problem.Variable;

public class MatrixManagerDynamicTest {

	MatrixManagerDynamic mmd;

	@Before
	public void setUp() throws Exception {
		final TupleSet ta = new TupleSet(false);
		ta.set(new int[] { 0, 0, 0 }, true);
		ta.set(new int[] { 1, 1, 1 }, true);
		ta.set(new int[] { 2, 2, 2 }, true);

		final int[] dom = new int[] { 0, 1, 2 };

		final Variable[] scope = { new Variable(dom), new Variable(dom),
				new Variable(dom) };

		mmd = new MatrixManagerDynamic(scope, ta, false);
	}

	@Test
	public void testRestore() {
		final Iterator<int[]> itr2 = mmd.iterator();
		itr2.next();
		itr2.next();
		itr2.remove();
		itr2.next();
		itr2.remove();
		assertTrue(mmd.isTrue(new int[] { 1, 1, 1 }));
		assertFalse(mmd.isTrue(new int[] { 0, 0, 0 }));
		assertFalse(mmd.isTrue(new int[] { 2, 2, 2 }));

	}

	@Test
	public void testIterator() {
		{
			final Iterator<int[]> itr1 = mmd.iterator();
			assertTrue(itr1.hasNext());
			assertArrayEquals(new int[] { 1, 1, 1 }, itr1.next());
			assertTrue(itr1.hasNext());
			assertArrayEquals(new int[] { 0, 0, 0 }, itr1.next());
			assertTrue(itr1.hasNext());
			assertArrayEquals(new int[] { 2, 2, 2 }, itr1.next());
			assertFalse(itr1.hasNext());
		}
		{
			final Iterator<int[]> itr2 = mmd.iterator();
			assertArrayEquals(new int[] { 1, 1, 1 }, itr2.next());
			itr2.remove();
			assertTrue(itr2.hasNext());
			assertArrayEquals(new int[] { 0, 0, 0 }, itr2.next());
			assertArrayEquals(new int[] { 2, 2, 2 }, itr2.next());
			assertFalse(itr2.hasNext());
		}
		{
			final Iterator<int[]> itr2 = mmd.iterator();
			assertArrayEquals(new int[] { 0, 0, 0 }, itr2.next());
			assertArrayEquals(new int[] { 2, 2, 2 }, itr2.next());
			itr2.remove();
			assertFalse(itr2.hasNext());

		}
		{
			final Iterator<int[]> itr2 = mmd.iterator();
			assertArrayEquals(new int[] { 0, 0, 0 }, itr2.next());
			itr2.remove();
			assertFalse(itr2.hasNext());
		}
		{
			final Iterator<int[]> itr2 = mmd.iterator();
			assertFalse(itr2.hasNext());
		}
	}

	@Test
	public void testClone() {
		fail("Not yet implemented");
	}

	@Test
	public void testRemove() {
		fail("Not yet implemented");
	}

	@Test
	public void testToString() {
		fail("Not yet implemented");
	}

}
