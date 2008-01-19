package cspfj.constraint;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.Arrays;

import org.junit.Before;
import org.junit.Test;

import cspfj.problem.Variable;

public class MatrixManager2DTest {

	private MatrixManager2D matrix;

	private int[] tuple;

	@Before
	public void setUp() throws Exception {
		final int[] dom = new int[] { 1, 2, 3 };
		final Variable v1 = new Variable(dom, "v1");
		final Variable v2 = new Variable(dom, "v2");
		tuple = new int[2];
		matrix = new MatrixManager2D(new Variable[] { v1, v2 }, tuple, null);
		matrix.generate(new Variable[] { v1, v2 }, true, new int[][] {
				{ 1, 1 }, { 1, 3 } }, 2);
	}

	@Test
	public void testHasSupport() {
		assertTrue("(0,0)", matrix.hasSupport(0, 0));
		assertFalse("(0,1)", matrix.hasSupport(0, 1));
		assertFalse("(0,2)", matrix.hasSupport(0, 2));
		assertTrue("(1,0)", matrix.hasSupport(1, 0));
		assertFalse("(1,1)", matrix.hasSupport(1, 1));
		assertTrue("(1,2)", matrix.hasSupport(1, 2));
	}

	@Test
	public void testSetFirstTuple() {
		tuple[1] = -1;
		assertTrue("(0,0)", matrix.setFirstTuple(0, 0));
		assertSame("First tuple of (0,0)", tuple[1], 0);

		assertFalse("(0,1)", matrix.setFirstTuple(0, 1));
		
		tuple[0] = -1;
		assertTrue("(1,2)", matrix.setFirstTuple(1, 2));
		assertSame("First tuple of (1,2)", tuple[0], 0);
		
		

	}

	@Test
	public void testNext() {
		tuple[1]= -1;
		assertTrue("(0,0)", matrix.setFirstTuple(0, 0));
		assertSame("First support of (0,0)", tuple[1], 0);
		
		assertTrue("next", matrix.next());
		assertSame("Second support of (0,0)", tuple[1], 2);
		
		assertFalse("end", matrix.next());
	}

	@Test
	public void testCheck() {
		tuple[0] = 0;
		tuple[1] = 1;
		assertFalse(Arrays.toString(tuple) + " should be false", matrix.check());
		tuple[1] = 2;
		assertTrue(Arrays.toString(tuple) + " should be true", matrix.check());
	}

}
