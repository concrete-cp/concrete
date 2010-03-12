package cspfj.constraint.extension;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;

import org.junit.Before;
import org.junit.Test;

import cspfj.problem.IntVariable;
import cspom.variable.CSPOMVariable;

public class MatrixManager2DTest {

	private MatrixManager2D matrix;

	private int[] tuple;

	@Before
	public void setUp() throws Exception {
		final IntVariable var1 = new IntVariable(new CSPOMVariable(1, 3));
		final IntVariable var2 = new IntVariable(new CSPOMVariable(1, 4));
		tuple = new int[2];

		final Matrix2D matrix2d = new Matrix2D(3, 4, false);
		matrix2d.set(new int[] { 0, 0 }, true);
		matrix2d.set(new int[] { 0, 2 }, true);

		matrix = new MatrixManager2D(new IntVariable[] { var1, var2 }, matrix2d,
				false);
		matrix.setTuple(tuple);
	}

	@Test
	public void testHasSupport() {
		assertTrue("(0,0)", matrix.hasSupport(0, 0));
		assertFalse("(0,1)", matrix.hasSupport(0, 1));
		assertFalse("(0,2)", matrix.hasSupport(0, 2));
		assertTrue("(1,0)", matrix.hasSupport(1, 0));
		assertFalse("(1,1)", matrix.hasSupport(1, 1));
		assertTrue("(1,2)", matrix.hasSupport(1, 2));
		assertFalse("(1,3)", matrix.hasSupport(1, 3));
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
