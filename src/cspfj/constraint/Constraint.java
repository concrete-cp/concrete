package cspfj.constraint;

import java.util.Collection;

import cspfj.filter.RevisionHandler;
import cspfj.problem.Variable;

public interface Constraint {
	int getArity();

	boolean isInvolved(final Variable variable);

	Variable getVariable(final int position);

	boolean checkFirst();

	int getId();

	Variable[] getScope();

	int getPositionInVariable(final int variablePosition);

	int getPosition(final Variable variable);

	boolean checkFirstWith(final int variablePos, final int index);

	boolean isActive();

	void setActive(final boolean b);

	void setPositionInVariable(final int variablePosition,
			final int constraintPosition);

	Constraint deepCopy(final Collection<Variable> variables)
			throws CloneNotSupportedException;

	void initNbSupports() throws InterruptedException;

	long getNbSupports(final Variable variable, final int index);

	// boolean skipRevision(int i);

	boolean revise(final int level, final RevisionHandler revisator);

	// int getOtherSize(int position);

	long getNbMaxConflicts(final int position);

	long getNbInitConflicts(final int position, final int index);

	boolean isBound();

	String getType();

	boolean check();

	void restore(int level);

	boolean isSlow();

	long getInitSize();

	int getWeight();

	void incWeight();

	void setWeight(final int weight);

	boolean getRemovals(int position);

	void setRemovals(int position, boolean value);

	void fillRemovals(final boolean value);

	// boolean findValidTuple(int position, int index);
}