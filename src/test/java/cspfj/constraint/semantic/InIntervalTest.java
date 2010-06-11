package cspfj.constraint.semantic;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertTrue;

import org.junit.Before;
import org.junit.Test;

import cspfj.constraint.Constraint;
import cspfj.filter.RevisionHandler;
import cspfj.problem.BitVectorDomain;
import cspfj.problem.Domain;
import cspfj.problem.Variable;

public final class InIntervalTest {

	private Domain domain;
	private InInterval constraint;

	@Before
	public void setUp() throws Exception {
		domain = new BitVectorDomain(1, 2, 3);
		final Variable var = new Variable("test", domain);
		constraint = InInterval.values(var, 2, 4);
	}

	@Test
	public void testIsConsistent() {
		assertTrue(constraint.isConsistent(0));
	}

	@Test
	public void testRevise() {
		assertTrue(constraint.revise(new RevisionHandler() {
			@Override
			public void revised(final Constraint constraint,
					final Variable variable) {
			}
		}, 0));
		assertArrayEquals(new int[] { 2, 3 }, domain.currentValues());
	}

}
