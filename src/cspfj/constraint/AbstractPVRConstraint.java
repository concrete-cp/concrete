package cspfj.constraint;

import java.util.Arrays;

import cspfj.problem.Variable;

public abstract class AbstractPVRConstraint extends AbstractConstraint {


	private boolean[] removals;
	
	
	public AbstractPVRConstraint(final Variable[] scope) {
		super(scope);
		//removals = new boolean[getArity()];
	}
	
	public void setRemovals(final boolean[] removals) {
		this.removals = removals;
	}
	
	public AbstractPVRConstraint(final Variable[] scope, final String name) {
		super(scope, name);
		removals = new boolean[getArity()];
	}

	public abstract boolean revise(final int position, final int level);

	public boolean hasRemovals(final int position) {
		return removals[position];
	}
	
	public void clearRemovals() {
		Arrays.fill(removals, false);
	}
	
	public void clearRemovals(final int position) {
		removals[position]=true;
	}
	
	public void setRemovals(int position) {
		removals[position]=true;
	}
	
	@Override
	public boolean revise(final int level, final boolean[] revised) {
		for (int i = getArity(); --i >= 0;) {
			final Variable y = getVariable(i);
			if (!y.isAssigned() && !supportCondition(i)
					&& !skipRevision(i) && revise(i, level)) {
				revised[i] = true;
				if (y.getDomainSize() <= 0) {
					return false;
				}
			}
		}
		return true;
	}

	private boolean skipRevision(final int variablePosition) {
//		if (!removals[variablePosition]) {
//			return false;
//		}

		for (int y = getArity(); --y >= 0;) {
			if (y == variablePosition ^ removals[y]) {
				return false;
			}
		}

		return true;
	}

}
