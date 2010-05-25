package cspfj.constraint;

import java.util.Set;

import cspfj.filter.RevisionHandler;
import cspfj.priorityqueues.Identified;
import cspfj.problem.Variable;

public interface Constraint extends Identified {
	/**
	 * @return the arity of the constraint (number of variables involved by the
	 *         constraint)
	 */
	int getArity();

	/**
	 * @param variable
	 * @return true iff the given variable is involved by the constraint
	 */
	boolean isInvolved(final Variable variable);

	/**
	 * @param position
	 * @return the variable at the given position
	 */
	Variable getVariable(final int position);

	/**
	 * @return the ordered scope of the constraint
	 */
	Variable[] getScope();

	/**
	 * @return the scope of the constraint as an unordered Set
	 */
	Set<Variable> getScopeSet();

	/**
	 * @param variable
	 * @return the position of the given variable. /!\ implementation in O(k)
	 */
	int getPosition(final Variable variable);

	/**
	 * The constraint propagator.
	 * 
	 * @param revisator
	 * @return false iff an inconsistency has been detected
	 */
	boolean revise(final RevisionHandler revisator, int reviseCount);

	/**
	 * @return string description of the constraint
	 */
	String getType();

	/**
	 * @return true iff the constraint is satisfied by the current values of the
	 *         tuple associated to the constraint object (see getTuple(int[]))
	 */
	boolean check();

	int[] getTuple();

	void setLevel(int level);

	void restore(int level);

	int getWeight();

	void incWeight();

	int getRemovals(int position);

	void setRemovals(int position, int value);

	void fillRemovals(final int value);

	boolean hasNoRemovals(final int reviseCount);

	int getEvaluation(final int reviseCount);

	boolean isEntailed();
	
	boolean isConsistent(final int reviseCount);

	void entail();
}