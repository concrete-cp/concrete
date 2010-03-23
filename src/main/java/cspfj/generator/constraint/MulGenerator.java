package cspfj.generator.constraint;

import java.util.Arrays;
import java.util.SortedSet;
import java.util.TreeSet;

import cspfj.constraint.semantic.Mul;
import cspfj.exception.FailedGenerationException;
import cspfj.problem.BitVectorDomain;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspfj.util.IntLinkedList;
import cspom.constraint.CSPOMConstraint;

public final class MulGenerator extends AbstractGenerator {

	public MulGenerator(final Problem problem) {
		super(problem);
	}

	@Override
	public boolean generate(final CSPOMConstraint constraint)
			throws FailedGenerationException {
		final Variable result = getSolverVariable(constraint.getVariable(0));
		final Variable v0 = getSolverVariable(constraint.getVariable(1));
		final Variable v1 = getSolverVariable(constraint.getVariable(2));

		int nulls = 0;
		for (Variable v : Arrays.asList(result, v0, v1)) {
			if (v.getDomain() == null) {
				nulls++;
			}
		}

		if (nulls > 1) {
			return false;
		} else if (nulls == 1) {
			if (result.getDomain() == null) {
				final SortedSet<Integer> values = new TreeSet<Integer>();
				for (int i : v0.getDomain().allValues()) {
					for (int j : v1.getDomain().allValues()) {
						values.add(i * j);
					}
				}
				result.setDomain(new BitVectorDomain(IntLinkedList
						.intCollectionToArray(values)));
			} else if (v0.getDomain() == null) {
				final SortedSet<Integer> values = new TreeSet<Integer>();
				for (int i : result.getDomain().allValues()) {
					for (int j : v1.getDomain().allValues()) {
						if (i % j == 0) {
							values.add(i / j);
						}
					}
				}
				v0.setDomain(new BitVectorDomain(IntLinkedList
						.intCollectionToArray(values)));
			} else if (v1.getDomain() == null) {
				final SortedSet<Integer> values = new TreeSet<Integer>();
				for (int i : result.getDomain().allValues()) {
					for (int j : v0.getDomain().allValues()) {
						if (i % j == 0) {
							values.add(i / j);
						}
					}
				}
				v1.setDomain(new BitVectorDomain(IntLinkedList
						.intCollectionToArray(values)));
			} else {
				throw new IllegalStateException();
			}
		}
		addConstraint(new Mul(result, v0, v1));
		return true;
	}

}
