package cspfj.constraint.semantic;

import static org.junit.Assert.fail;

import java.util.Random;
import java.util.SortedSet;
import java.util.TreeSet;

import org.junit.Before;
import org.junit.Test;

import cspfj.constraint.AbstractAC3Constraint;
import cspfj.constraint.Constraint;
import cspfj.filter.RevisionHandler;
import cspfj.problem.BitVectorDomain;
import cspfj.problem.Variable;

public final class AbsDiffTest {

	private static final Random RAND = new Random();
	private Variable x, y, z;
	private int[] domX, domY, domZ;

	@Before
	public void setUp() throws Exception {
		domX = randomDomain(-100, 100, 20);
		domY = randomDomain(-100, 100, 20);
		domZ = randomDomain(-100, 100, 20);
		x = new Variable("x", new BitVectorDomain(domX));
		y = new Variable("y", new BitVectorDomain(domY));
		z = new Variable("z", new BitVectorDomain(domZ));
	}

	@Test
	public void testReviseInt() {
		final Constraint c = new AbsDiff(x, y, z);
		c.revise(new RevisionHandler() {
			@Override
			public void revised(final Constraint constraint,
					final Variable variable) {
			}
		}, 0);
		final Constraint c2 = new AbstractAC3Constraint(x, y, z) {
			@Override
			public boolean check() {
				return getValue(0) == Math.abs(getValue(1) - getValue(2));
			}

			@Override
			public int getEvaluation(final int reviseCount) {
				return 0;
			}
		};
		c2.revise(new RevisionHandler() {
			@Override
			public void revised(final Constraint constraint,
					final Variable variable) {
				fail("Revised " + variable);
			}
		}, 0);

	}

	private static int[] randomDomain(final int min, final int max, final int nb) {
		final SortedSet<Integer> domain = new TreeSet<Integer>();
		while (domain.size() < nb) {
			domain.add(RAND.nextInt(max - min) + min);
		}
		final int[] finalDomain = new int[nb];
		int i = 0;
		for (int v : domain) {
			finalDomain[i++] = v;
		}
		return finalDomain;
	}

}
