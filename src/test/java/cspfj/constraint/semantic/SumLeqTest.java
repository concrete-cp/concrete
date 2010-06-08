package cspfj.constraint.semantic;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertTrue;

import org.junit.Before;
import org.junit.Test;

import cspfj.constraint.Constraint;
import cspfj.filter.RevisionHandler;
import cspfj.problem.BitVectorDomain;
import cspfj.problem.Variable;

public final class SumLeqTest {

	private Variable v0;
	private Variable v1;
	private Constraint c;

	@Before
	public void setUp() throws Exception {
		v0 = new Variable("v0", new BitVectorDomain(1, 2, 3, 4));
		v1 = new Variable("v1", new BitVectorDomain(0, 1, 2, 3, 4));
		c = new SumLeq(4, v0, v1);
	}

	@Test
	public void reviseTest() {
		assertTrue(c.revise(new RevisionHandler() {
			@Override
			public void revised(final Constraint constraint,
					final Variable variable) {
			}
		}, 0));

		assertArrayEquals(new int[] { 1, 2, 3, 4 }, v0.getDomain()
				.currentValues());
		assertArrayEquals(new int[] { 0, 1, 2, 3 }, v1.getDomain()
				.currentValues());
	}

}
