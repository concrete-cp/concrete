package cspfj.constraint;

import java.util.Collection;
import java.util.Set;

import cspfj.problem.Variable;

public interface Constraint {
	int getArity();

	boolean isInvolved(Variable variable);

	Variable getVariable(int position);

	boolean checkFirst();

	int getId();

	Variable[] getScope();

	int getPositionInVariable(int variablePosition);

	int getPosition(Variable variable);

	boolean checkFirstWith(int variablePos, int index);

	boolean isActive();

	void setActive(boolean b);

	void setPositionInVariable(int variablePosition, int constraintPosition);

	Constraint deepCopy(Collection<Variable> variables)
			throws CloneNotSupportedException;

	void initNbSupports() throws InterruptedException;

	long getNbSupports(Variable variable, int index);

	// boolean skipRevision(int i);

	boolean revise(int level, boolean[] revised);

//	int getOtherSize(int position);

	long getNbMaxConflicts(int position);

	long getNbInitConflicts(int position, int index);

	boolean isBound();

	String getType();
	
	boolean check();

	void restore(int level);

	boolean isSlow();
	
	long getSize();

	//boolean findValidTuple(int position, int index);
}