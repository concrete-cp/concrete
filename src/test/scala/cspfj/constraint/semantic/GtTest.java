package cspfj.constraint.semantic;

import static org.junit.Assert.assertArrayEquals;

import org.junit.Before;
import org.junit.Test;

import cspfj.constraint.Constraint;
import cspfj.filter.RevisionHandler;
import cspfj.problem.BitVectorDomain;
import cspfj.problem.Variable;

public final class GtTest {
	Variable v1;
	Variable v2;

	@Before
	public void setUp() throws Exception {
		v1 = new Variable("v1", new BitVectorDomain(1, 2, 3, 4));
		v2 = new Variable("v2", new BitVectorDomain(3, 4, 5));
	}

	@Test
	public void testRevise1() {

		final Constraint c = new Gt(v1, v2, true);
		c.revise(new RevisionHandler() {
			@Override
			public void revised(final Constraint constraint,
					final Variable variable) {
			}
		}, 0);

		assertArrayEquals(new int[] { 4 }, v1.getDomain().currentValues());
		assertArrayEquals(new int[] { 3 }, v2.getDomain().currentValues());

	}

	@Test
	public void testRevise2() {

		final Constraint c = new Gt(v1, v2, false);
		c.revise(new RevisionHandler() {
			@Override
			public void revised(final Constraint constraint,
					final Variable variable) {
			}
		}, 0);

		assertArrayEquals(new int[] { 3, 4 }, v1.getDomain().currentValues());
		assertArrayEquals(new int[] { 3, 4 }, v2.getDomain().currentValues());

	}

}
