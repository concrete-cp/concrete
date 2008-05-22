package cspfj.constraint;

import java.util.Collection;

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
	Constraint deepCopy(Collection<Variable> variables) throws CloneNotSupportedException;
	void initNbSupports() throws InterruptedException;
	boolean findValidTuple(int i, int index);
	long getNbSupports(Variable variable, int index);
	boolean skipRevision(int i);
	boolean revise(int i, int level);
	int getOtherSize(int position);
	long getNbMaxConflicts(int position);
	long getNbInitConflicts(int position, int index);
	boolean isBound();
	int[] getTuple();
	String getType();
}