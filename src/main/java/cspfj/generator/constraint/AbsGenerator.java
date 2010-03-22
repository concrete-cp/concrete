package cspfj.generator.constraint;

import java.util.SortedSet;
import java.util.TreeSet;

import cspfj.constraint.semantic.Abs;
import cspfj.exception.FailedGenerationException;
import cspfj.problem.BitVectorDomain;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspfj.util.IntLinkedList;
import cspom.constraint.CSPOMConstraint;

public class AbsGenerator extends AbstractGenerator {

	public AbsGenerator(Problem problem) {
		super(problem);
	}

	@Override
	public boolean generate(CSPOMConstraint constraint)
			throws FailedGenerationException {
		final Variable v0 = getSolverVariable(constraint.getVariable(1));
		final Variable result = getSolverVariable(constraint.getVariable(0));

		if (v0.getDomain() == null) {

			if (result.getDomain() == null) {
				return false;
			}

			final SortedSet<Integer> values = new TreeSet<Integer>();
			for (int i : result.getDomain().allValues()) {
				values.add(i);
				values.add(-i);
			}
			v0.setDomain(new BitVectorDomain(IntLinkedList
					.intCollectionToArray(values)));

		} else if (result.getDomain() == null) {

			final SortedSet<Integer> values = new TreeSet<Integer>();
			for (int i : v0.getDomain().allValues()) {
				values.add(Math.abs(i));
			}
			result.setDomain(new BitVectorDomain(IntLinkedList
					.intCollectionToArray(values)));

		}
		addConstraint(new Abs(result, v0));
		return true;

	}

}
