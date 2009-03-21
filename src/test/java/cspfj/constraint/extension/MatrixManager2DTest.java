package cspfj.constraint.extension;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;

import org.junit.Before;
import org.junit.Test;

import cspfj.constraint.extension.Matrix2D;
import cspfj.constraint.extension.MatrixManager2D;
import cspfj.problem.Variable;

public class MatrixManager2DTest {

	private MatrixManager2D matrix;

	private int[] tuple;

	@Before
	public void setUp() throws Exception {
		final int[] dom1 = new int[] { 1, 2, 3 };
		final Variable var1 = new Variable(dom1, "v1");
		final int[] dom2 = new int[] { 1, 2, 3, 4 };
		final Variable var2 = new Variable(dom2, "v2");
		tuple = new int[2];

		final Matrix2D matrix2d = new Matrix2D(3, 4, false);
		matrix2d.set(new int[] { 0, 0 }, true);
		matrix2d.set(new int[] { 0, 2 }, true);

		matrix = new MatrixManager2D(new Variable[] { var1, var2 }, matrix2d, false);
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
